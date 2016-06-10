{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Conduit as C
import Network.HTTP.Types as T
import Network.HTTP.Client.Conduit

import Data.Char
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import System.IO
--import Data.Text 
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Internal
import Data.ByteString.Char8 (pack)


jsonFile :: FilePath
jsonFile = "./lps_json.json"


data UdcmCode = UdcmCode { udcmCode :: String} deriving (Show, Generic)



getUDCM :: UdcmCode -> String
getUDCM (UdcmCode {udcmCode = u}) = u

--data RedisRequest = RedisRequest{ locationCode :: L.ByteString } deriving (Show, Generic)

data UserState = UserState {
                            udcm :: String,
                            timestamp :: Integer,
                            locationCode :: String,
                            betaConfidence :: Float,
                            gammaConfidence :: Float,
                            predictionLevel :: String,
                            nearTagLocationCode :: String,
                            nearTagConfidence :: Float
                           } deriving (Show, Generic)

class RedisWritable a where
    writeForm :: a -> [(ByteString, ByteString)]
    genKey :: a -> ByteString

instance RedisWritable UserState where
    writeForm st = [ (pack y, pack z) | (y, z) <-  h ] where
        h = [("udcm", udcm st), ("timestamp", show $ timestamp st),
                    ("locationCode", locationCode st),
                    ("betaConfidence", show $ betaConfidence st),
                    ("gammaConfidence", show $ gammaConfidence st),
                    ("predictionLevel", predictionLevel st),
                    ("nearTagLocationCode", nearTagLocationCode st),
                    ("nearTagConfidence", show $ nearTagConfidence st)]
    genKey st = pack $ udcm st ++ ":" ++ (show $ timestamp st)


instance FromJSON UdcmCode
--instance ToJSON RedisRequest


main = do
       lpsUrl <- parseUrl "http://server.development.com:9040/service/lps/debugLocation"
       manager <- C.newManager C.tlsManagerSettings
       contents <- B.readFile "./lps_json.json"
       let a (Just x) = x
       let b (T.Response y) = y
--       print (decode contents :: Maybe UdcmCode)
       print (getUDCM . a $ (decode contents :: Maybe UdcmCode))
       let req = lpsUrl { method = methodPost
                         , requestHeaders = [("Content-Type", "application/json")]
                         , requestBody = RequestBodyLBS contents
                         }
       res <- C.httpLbs req manager
 --      print (eitherDecode $ responseBody res)
       L.putStrLn $ responseBody res
       print res
       
