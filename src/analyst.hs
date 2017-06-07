#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment
import System.Exit
import Data.Map as M
import Network.AMQP
import Network.AMQP.Types
import Data.Char
import Data.List    -- for intercalate

main = do
    specs <- getArgs

    if length specs /= 1 then do
        putStrLn "Give 1 specialization !"
        putStr   "("
        putStr   (intercalate " / " specializations)
        putStrLn ")"
        exitFailure
    else
        putStr ""

    (bodyPart:_) <- getArgs
    arg0 : _ <- getArgs

    putStrLn "+ --------- +"
    putStr   "|  ANALYST  | ("
    putStr   (fmap toUpper arg0)
    putStrLn ")\n+ --------- +"

    -- Connection
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    -- Get declared Queue (created in tech)
    (replyToQ, _, _) <- declareQueue chan anonQueue

    -- Declare Exchange
    declareExchange chan newExchange {exchangeName = "hospital-exchange", exchangeType = "direct"}

    putStrLn "Running... Press Enter to close.\n"


    -- Publish to the Queue
    publishMsg chan "hospital-exchange" "" (msg replyToQ bodyPart)
    consumeMsgs chan replyToQ Ack acceptReply


    getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"

  where
    specializations = ["analysis", "algebra", "logic"]
    msg rk bodyPart = newMsg { msgBody = (BL.pack $ bodyPart),
                                msgReplyTo = Just rk }

anonQueue :: QueueOpts
anonQueue = QueueOpts "" False False True True (FieldTable M.empty)
-- so the queue does not remain active when a server restarts

acceptReply :: (Message, Envelope) -> IO ()
acceptReply (msg, env) = do
    putStrLn $ ">> " ++ (BL.unpack $ msgBody msg)
    ackEnv env
