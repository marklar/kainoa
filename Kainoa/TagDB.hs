module Kainoa.TagDB
(
) where

import Database.HDBC
import Database.HDBC.MySQL

{-
  HDBC:
    - http://book.realworldhaskell.org/read/using-databases.html
    - http://software.complete.org/static/hdbc/doc/Database-HDBC.html
  MySQL:                                                        
    - http://hackage.haskell.org/packages/archive/HDBC-mysql/0.6/doc/html/Database-HDBC-MySQL.html

  Use InnoDB tables (which support transactions).               
-}

-- Cfg --
dbHost     = "db-host"
dbUser     = "mark"
dbPort     = 3306
schemaName = "tags" -- ???

dbHandle :: IO Connection.Connection                         
dbHandle schemaName =
    connectMySQL defaultMySQLConnectInfo {
                       mysqlHost     = dbHost
                     , mysqlUser     = dbUser
                     , mysqlPort     = dbPort
                     , mysqlDatabase = schemaName
                     }

prepareDb :: IO ()
prepareDb = do
  dbh <- dbHandle dbSchema
  createTables dbh
  disconnect dbh
