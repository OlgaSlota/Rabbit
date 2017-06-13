#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package amqp
    --package bytestring
    --package safe
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import           Safe (atMay)
import           System.Environment (getArgs)

mathExchange = "direct_math"

main :: IO ()
main = do
     args  <- getArgs
     let body           = BL.concat [(methodFor args), (BL.pack " "), (bodyFor args)]
         specialization = specializationFor args
     conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
     ch    <- openChannel conn

     declareExchange ch newExchange {exchangeName    = mathExchange,
                                     exchangeType    = "direct",
                                     exchangeDurable = False}
     publishMsg ch mathExchange specialization
                (newMsg {msgBody = body,
                         msgDeliveryMode = Just NonPersistent})

     BL.putStrLn $ " [x] Sent " <> body
     closeConnection conn

-- (+++) :: Maybe -> Maybe -> Maybe -> Maybe
(+++) a b c = (++) <$> ((++) <$> a <*> b) <*> c

specializationFor :: [String] -> DT.Text
specializationFor xs = maybe "analysis" DT.pack (atMay xs 0)

methodFor :: [String] -> BL.ByteString
methodFor xs = maybe "ack" BL.pack (atMay xs 1)

bodyFor :: [String] -> BL.ByteString
bodyFor xs =
    case nrOfParams of
        1 -> maybe "4" BL.pack (atMay xs 2)
        2 -> maybe "4 3" BL.pack ((+++) (atMay xs 2) (Just " ") (atMay xs 3))
        _ -> BL.pack ""
    where
        nrOfParams :: Int
        nrOfParams = (length xs) - 2
