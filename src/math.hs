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
import Data.Maybe (fromJust)
import Data.Map as M
import Network.AMQP
import Data.Char as C
import Data.Text as T
import Data.List as L
import Analysis

main = do
    intro

    qName:_ <- getArgs
    -- Connection
    conn      <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan      <- openChannel conn
    replyChan <- openChannel conn

    -- Declare Queue
    declareQueue chan newQueue {queueName = T.pack(qName)}
    -- Declare Exchange
    declareExchange chan newExchange {exchangeName = "math-exchange", exchangeType = "direct"}

    bindQueue chan (T.pack(qName)) "math-exchange" ""

    putStrLn "/press Enter to close.../\n"

    -- Subscribe to the Queue
    consumeMsgs chan (T.pack(qName)) Ack (processMessage replyChan)


    getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"

--processMessage :: Channel -> (Message, Envelope) -> IO ()
processMessage chan (msg, env) = do
  arg0 : _ <- getArgs
  if bodyPart == arg0 then do
      publishMsg chan "" routingKey reply
      ackEnv env
  else do
      putStr   "I cannot help with `"
      putStr   bodyPart
      putStrLn  "` problem :("

  where
    -- extract routing key
    routingKey = fromJust $ msgReplyTo msg
    -- build reply
    reply = newMsg {msgBody = (BL.pack (getReply bodyPart))}
    -- extract body part
    bodyPart = BL.unpack $ msgBody msg
    solutions = M.fromList[("analysis", "Analysis done"), ("algebra", "Algebra done"), ("logic", "Logic done")]
    -- lookup in the Map
    getReply bodyPart =
      case M.lookup bodyPart solutions of
        Just v  -> v
        Nothing -> ""

intro = do
--     specs <- getArgs
--
--     if length specs /= 1 then do
--         putStr "Give 1 specialization !\n("
--         putStr   (intercalate " / " specializations)
--         putStrLn ")"
--         exitFailure
--     else
--         putStr ""

    arg0 : _ <- getArgs

    if arg0 `notElem` specializations then do
        putStr "Please give a specialization from the list:\n("
        putStr   (L.intercalate " / " specializations)
        putStrLn ")"
        exitFailure
    else do
        putStrLn "+ --------------- +"
        putStr   "|  MATHEMATICIAN  | ("
        putStr   (fmap C.toUpper arg0)
        putStrLn ")\n+ --------------- +"

  where
    specializations = ["analysis", "algebra", "logic"]
