{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}                                        
import Data.Maybe
import Data.Aeson
import Data.Text hiding (foldl)
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale

data Upcoming = Upcoming { totalCount :: Int, entries :: [UpcomingEntry] } deriving (Show, Generic)

data UpcomingEntry = UpcomingEntry { channel :: !Text, title :: !Text, start :: Int } deriving (Show, Generic)

instance FromJSON Upcoming 
        
instance FromJSON UpcomingEntry 

jsonURL :: String
jsonURL = "http://localhost:9981/dvrlist_finished"

getJSON :: IO BS.ByteString
getJSON = simpleHttp jsonURL

result :: IO (Maybe Upcoming)
result = do v <- getJSON
            return (decode v :: Maybe Upcoming)

getTime :: Int -> UTCTime
getTime secs = posixSecondsToUTCTime $ fromIntegral secs

timeToString :: UTCTime -> String
timeToString time = formatTime defaultTimeLocale "%c" time 

getTitle :: UpcomingEntry -> String
getTitle UpcomingEntry { channel = ch, title = txt, start = st } = 
            printf "%s\n%s\n%s\n" (unpack ch) (unpack txt) (timeToString (getTime st))

titles :: Upcoming -> String
titles (Upcoming { entries = xs }) = foldl (\acc x -> acc ++ "\n" ++ getTitle x) "" xs


main :: IO ()
main =  do v <- result
           putStrLn (titles (fromJust v))
        

