{-
  Supports only RabbitMQ.
  http://hackage.haskell.org/packages/archive/amqp/0.1/doc/html/Network-AMQP.html
-}
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

main = do
  {-
    "/" == virtualHost
    Namespace for AMQP resources.
    Diff apps can use diff virtual hosts on same broker.
   -}
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"

  {-
    A connection can have multiple channels.
    All activity happens on channels.
   -}
  chan <- openChannel conn

  {-
    Queue.
    Pass it a QueueOpts (created via newQueue).
    Only queueName is required.
    Options w/ defaults:
      queuePassive    (False) -- T: won't *create* queue.
      queueDurable    (True)  -- T: remains active when server restarts.
      queueExclusive  (False) -- T: no other consumer may subscribe.
      queueAutoDelete (False) -- T: deletes when last subscriber exits.
   -}
  declareQueue chan $ newQueue {queueName = "myQueue"}

  -- Exchange.
  declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}

  -- Binding.
  bindQueue chan "myQueue" "myExchange" "myKey"

  -- subscribe to the queue
  consumeMsgs chan "myQueue" Ack myCallback

  -- publish a message to our new exchange
  publishMsg chan "myExchange" "myKey" 
      newMsg {msgBody = (BL.pack "hello world"), 
              msgDeliveryMode = Just Persistent}

  getLine -- wait for keypress
  closeConnection conn
  putStrLn "connection closed"

    
myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
  putStrLn $ "received message: "++(BL.unpack $ msgBody msg)
  -- acknowledge receiving the message
  ackEnv env
