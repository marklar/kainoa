{-
  Supports only RabbitMQ.
  http://hackage.haskell.org/packages/archive/amqp/0.1/doc/html/Network-AMQP.html
-}
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

main = do
  {-
    CONNECTION.
    "/" == virtualHost
    Namespace for AMQP resources.
    Diff apps can use diff virtual hosts on same broker.
   -}
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"

  {-
    CHANNEL.
    A connection can have multiple channels.
    All activity happens on channels.
   -}
  chan <- openChannel conn

  {-
    QUEUE.
    Pass it a QueueOpts (created via newQueue).
    Durable (by default).
    Qs do not have binding keys intrinsically;
    it is a Q's binding to an X which has a binding key.

    Options w/ defaults:
      queueName: required
      queuePassive    (False) -- T: won't *create* queue.
      queueDurable    (True)  -- T: remains active when server restarts.
      queueExclusive  (False) -- T: no other consumer may subscribe.
      queueAutoDelete (False) -- T: deletes when last subscriber exits.
   -}
  declareQueue chan $ newQueue {queueName = "myQueue"}

  {-
    EXCHANGE.
    Direct.  So must provide "binding key"
    so X knows whether to pass Ms to this Q.
    This X is durable (by default).
  -}
  declareExchange chan newExchange { exchangeName = "myExchange"
                                   , exchangeType = "direct"
                                   }
  {-
    BIND Q->X.
    Binding key: "myKey".
    
    Binding is a separate step.
    Create the X and Q first, then bind them together.
  -}
  bindQueue chan "myQueue" "myExchange" "myKey"

  {-
    SUBSCRIBE to the queue.
    This doesn't happen automatically when one binds?  Strange.
    myCallback: fired upon receipt.
  -}
  consumeMsgs chan "myQueue" Ack myCallback

  {-
    PUBLISH to X.
    Routing key "myKey" matches binding key for our Q.
    Delivery mode: persistent.
  -}
  publishMsg chan "myExchange" "myKey" 
      newMsg { msgBody = BL.pack "hello world"
             , msgDeliveryMode = Just Persistent
             }

  getLine -- wait for keypress
  closeConnection conn
  putStrLn "connection closed"

    
myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn $ "received message: "++(BL.unpack $ msgBody msg)
  -- acknowledge receiving the message
  ackEnv env
