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
import Data.Char as C
import Data.List as L
import Data.Text as T

main = do
    intro

    (bodyPart:_) <- getArgs

    -- Connection
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    -- Declare queue to receive reply from math.hs
    (replyQueue, _, _) <- declareQueue chan anonQueue

    -- Declare Exchange
    declareExchange chan newExchange {exchangeName = "math-exchange", exchangeType = "direct"}

    putStrLn "/press Enter to close.../\n"


    -- Publish to the Queue
    publishMsg chan "math-exchange" "" (msg replyQueue bodyPart)
    consumeMsgs chan replyQueue Ack acceptReply


    getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"

  where
    msg rk bodyPart = newMsg { msgBody = (BL.pack $ bodyPart),
                                msgReplyTo = Just rk }

anonQueue :: QueueOpts
anonQueue = QueueOpts "" False False True True (FieldTable M.empty)
-- so the queue does not remain active when a server restarts

acceptReply :: (Message, Envelope) -> IO ()
acceptReply (msg, env) = do
    putStrLn $ ">> " ++ (BL.unpack $ msgBody msg)
    ackEnv env

intro = do
--     specs <- getArgs
--
--     if length specs /= 1 then do
--         putStr "Give 1 specialization !\n("
--         putStr   (intercalate " / " specializations)
--         putStrLn ")"
--         exitFailure
--     else
--         putStr ""   -- I do not know how to remove else

    arg0 : _ <- getArgs

    if arg0 `notElem` specializations then do
        putStr "Please give a specialization from the list:\n("
        putStr   (L.intercalate " / " specializations)
        putStrLn ")"
        exitFailure
    else
        putStr ""

    putStrLn "+ --------- +"
    putStr   "|  ANALYST  | ("
    putStr   (fmap C.toUpper arg0)
    putStrLn ")\n+ --------- +"

  where
    specializations = ["analysis", "algebra", "logic"]
