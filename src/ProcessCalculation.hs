#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

module ProcessCalculation (processMsg) where

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
import           Text.Printf as P
import           System.Environment (getArgs)
import           Analysis
import           Algebra
import           Logic
import           Data.Maybe
import           Language.Haskell.Interpreter

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
  let result = processMsg (BL.unpack key) (S.splitOn " " (BL.unpack body))
  case isInt result of
    True -> P.printf "%.0f\n" result
    _ -> print result
  ackEnv metadata
  where
    body = msgBody msg
    key  = BL.fromStrict . DT.encodeUtf8 $ envRoutingKey metadata

isInt x = x == fromInteger (round x)

intro = do
    specs <- getArgs

    if DL.length specs < 1 then do
        putStr "Give at least 1 specialization !\n("
        putStr   (DL.intercalate " / " specializations)
        putStrLn ")"
        exitFailure
    else
        putStrLn ""
    where
        specializations = ["analysis", "algebra", "logic"]

processMsg :: String -> [String] -> Float
processMsg key body =
    case key of
        "analysis" -> funEval analysisSolution
        "algebra" -> funEval algebraSolution
        "logic" -> funEval logicSolution
        where
                  funEval solutions =
                      case M.lookup arg0 solutions of
                          Just v  -> v
                          Nothing -> 0

                  analysisSolution = M.fromList[ ("deriveSqrt", deriveSqrt arg1 arg2),
                                                 ("deriveSqr", deriveSqr arg1 arg2),
                                                 ("deriveCubic", deriveCubic arg1 arg2),
                                                 ("deriveTan", deriveTan arg1 arg2),
                                                 ("macLaurinEx", macLaurinEx arg1Int arg2)]
                  algebraSolution = M.fromList[ ("ack", fromIntegral (ack arg1Int arg2Int)),
                                                ("lawOfCosines", lawOfCosines arg1 arg2 arg3)]
                  logicSolution = M.fromList[ ("xor", fromIntegral (xor arg1Int arg2Int)),
                                              ("or'", fromIntegral (or' arg1Int arg2Int)),
                                              ("and'", fromIntegral (and' arg1Int arg2Int)),
                                              ("deMorgan", fromIntegral (deMorgan arg1Int arg2Int))]

                  arg0 = body !! 0
                  arg1 = read (body !! 1) :: Float
                  arg2 = read (body !! 2) :: Float
                  arg3 = read (body !! 3) :: Float
                  arg1Int = read (body !! 1) :: Int
                  arg2Int = read (body !! 2) :: Int
