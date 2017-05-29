#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)
import Data.Map as M

import Network.AMQP

main = do
    conn      <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan      <- openChannel conn
    replyChan <- openChannel conn

    --declare queues, exchanges and bindings
    declareQueue chan newQueue {queueName = "hospital-queue"}

    declareExchange chan newExchange {exchangeName = "hospital-exchange", exchangeType = "direct"}
    bindQueue chan "hospital-queue" "hospital-exchange" ""

    putStrLn "Running Technician. Press any key to close"

    --subscribe to the queues
    consumeMsgs chan "hospital-queue" Ack (processMessage replyChan)

    getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"

--processMessage :: Channel -> (Message, Envelope) -> IO ()
processMessage chan (msg, env) = do
  publishMsg chan "" routingKey reply
  ackEnv env

  where
    -- extract routing key
    routingKey = fromJust $ msgReplyTo msg
    -- build reply
    reply  = (newMsg {msgBody = (BL.pack (getReply bodyPart))})
    --extract body part
    bodyPart = BL.unpack $ msgBody msg
    injuries = M.fromList([("knee","Knee X-ray done"), ("ankle","Ankle X-ray done"), ("elbow","Elbow X-ray done")])
    -- lookup in the Map
    getReply bodyPart =
      case M.lookup bodyPart injuries of
        Just v  -> v
        Nothing -> "We cannot help with this injury"