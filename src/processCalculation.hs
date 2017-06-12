#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.AMQP
import           System.Exit
import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import           Data.Map as M
import qualified Data.Text as DT
import qualified Data.List as DL
import           Data.List.Split as S
import qualified Data.Text.Encoding as DT
import           System.Environment (getArgs)
import           Analysis
import           Algebra
import           Logic

mathExchange = "direct_math"

main :: IO ()
main = do
     intro

     conn       <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch         <- openChannel conn
     severities <- getArgs

     declareExchange ch newExchange {exchangeName    = mathExchange,
                                     exchangeType    = "direct",
                                     exchangeDurable = False}
     (q, _, _) <- declareQueue ch newQueue {queueName       = "",
                                            queueAutoDelete = True,
                                            queueDurable    = False}
     forM_ severities (bindQueue ch q mathExchange . DT.pack)

     BL.putStrLn " [*] Waiting for messages. To exit press CTRL+C"
     consumeMsgs ch q Ack deliveryHandler

     -- waits for keypresses
     getLine
     closeConnection conn

deliveryHandler :: (Message, Envelope) -> IO ()
deliveryHandler (msg, metadata) = do
  BL.putStrLn $ " [x] " <> key <> ":" <> body
  BL.putStr " [x] "
  print (processMsg (BL.unpack(key)) (S.splitOn " " (BL.unpack(body))))
  ackEnv metadata
  where
    body = msgBody msg
    key  = BL.fromStrict . DT.encodeUtf8 $ envRoutingKey metadata

intro = do
    specs <- getArgs

    if DL.length specs < 1 then do
        putStr "Give at least 1 specialization !\n("
        putStr   (DL.intercalate " / " specializations)
        putStrLn ")"
        exitFailure
    else do
        putStrLn ""
    where
        specializations = ["analysis", "algebra", "logic"]

processMsg :: String -> [String] -> Float
processMsg key body =
    case M.lookup key solutions of
            Just v  -> v
            Nothing -> 0
    where solutions = M.fromList[("analysis", derive arg1 (\x -> x^2) arg2 ), ("algebra", fromIntegral (ack arg1Int arg2Int )), ("logic",  fromIntegral (xor arg1Int arg2Int ))]
          arg1 = (read (body !!0) :: Float)
          arg2 = (read (body !!1) :: Float)
          arg1Int = (read (body !!0) :: Int)
          arg2Int = (read (body !!1) :: Int)
