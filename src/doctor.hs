#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import          System.Environment
import Data.Map as M
import Network.AMQP
import Network.AMQP.Types

main = do
    (bodyPart:_) <- getArgs

    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    (replyToQ, _, _) <- declareQueue chan anonQueue

    declareExchange chan newExchange {exchangeName = "hospital-exchange", exchangeType = "direct"}

    publishMsg chan "hospital-exchange" "" (msg replyToQ bodyPart)
    consumeMsgs chan replyToQ Ack acceptReply

    getLine -- wait for keypress

    closeConnection conn
    putStrLn "connection closed"

  where
    msg rk bodyPart = (newMsg { msgBody = (BL.pack $ bodyPart)
                  , msgReplyTo = Just rk })

anonQueue :: QueueOpts
anonQueue = QueueOpts "" False False True True (FieldTable M.empty)
-- so the queue does not remain active when a server restarts

acceptReply :: (Message, Envelope) -> IO ()
acceptReply (msg, env) = do
    putStrLn $ "Request result: " ++ (BL.unpack $ msgBody msg)
    ackEnv env