{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}                                        

import Data.Maybe
import Data.Aeson hiding (Value, String)
import Data.Text hiding (foldl, head)
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import System.Environment
import Data.Configurator
import Data.Configurator.Types
import Paths_PVRConsole
import Text.PrettyPrint.Boxes

data Entries = Entries { totalCount :: Int, 
                         entries :: [Entry] } 
                         deriving (Show, Generic)

data Entry = Entry { channel :: !Text, 
                     title :: !Text, 
                     description :: !Text,
                     start :: Int } 
                     deriving (Show, Generic)

instance FromJSON Entries 
        
instance FromJSON Entry 

jsonURL :: String -> String -> String
jsonURL host cmd = printf "http://%s:9981/%s" host cmd

getJSON :: String -> String -> IO BS.ByteString
getJSON host cmd = simpleHttp (jsonURL host cmd)

getPvrData :: String -> String -> IO (Maybe Entries)
getPvrData host cmd = do v <- getJSON host cmd
                         return (decode v :: Maybe Entries)

getTime :: Int -> UTCTime
getTime secs = posixSecondsToUTCTime $ fromIntegral secs

timeToString :: UTCTime -> String
timeToString time = formatTime defaultTimeLocale "%c" time 

getTitle :: Entry -> Box
getTitle entry = ch <+> tt <+> st // ds
               where ch = alignHoriz left 20   (text (unpack (channel entry)))
                     tt = alignHoriz left 40  (text (unpack (title entry)))
                     ds = para left 73 (unpack (description entry))
                     st = alignHoriz left 10  (text (timeToString (getTime (start entry))))
                       
titles :: Entries -> Box
titles xs = foldl f nullBox (entries xs)
          where f acc x = acc /+/ (getTitle x)

getString :: Value -> String
getString (String s) = unpack s
getString _ = error "Expected a string"

main :: IO ()
main =  do xs <- getArgs
           file <- getDataFileName "PVRConsole.cfg"
           cfg <- load [Required file]
           host <-require cfg "host" :: IO Value                                                             
           v <- getPvrData (getString host) (head xs)
           printBox (titles (fromJust v)) 
           return ()