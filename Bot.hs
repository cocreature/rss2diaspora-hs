{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Catch
import           Codec.Binary.UTF8.String (decode)
import           Config
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Diaspora
import           Network.HTTP.Conduit
import           RSS
import           Safe
import qualified System.IO.Strict as Strict
import           System.Environment
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.Feed.Types

main :: IO ()
main =
  do args <- getArgs
     if length args < 1
        then putStrLn "usage: ./rssbot path/to/config"
        else do conffile <- readConfigFile (head args)
                case conffile of
                  Right conf ->
                    forever $
                    flip runReaderT conf $
                    do rssStr <- lift $
                                 simpleHttp (rssURL conf) `catch`
                                 networkHandler
                       replaced <- replaceIfNecessary rssStr
                       case replaced of
                         Nothing -> lift sleep
                         Just text ->
                           do _ <- post text `catch`
                                   (lift . networkHandler)
                              lift sleep
                  Left e -> print e

networkHandler :: HttpException -> IO BL.ByteString
networkHandler e =
  print e >>
  return ""

sleep :: IO ()
sleep =
  threadDelay
    (300 * 10 ^
     (6 :: Int))

replaceIfNecessary :: BL.ByteString -> ReaderT ConfigFile IO (Maybe T.Text)
replaceIfNecessary rssStr =
  do let maybeItem = getFirstFeedItem rssStr
     case maybeItem of
       Nothing -> return Nothing
       Just item ->
         do let maybeId = getItemId item
            case maybeId of
              Nothing -> replaceFeedItem' item
              Just (_,itemId) ->
                replaceIfNew item itemId

readHandler :: IOError -> IO String
readHandler _ =
  putStrLn "feed item id could not be read" >>
  return ""

writeHandler :: IOError -> IO ()
writeHandler e = putStrLn $ "feed item id could not be written" ++ show e

replaceIfNew :: Item -> String -> ReaderT ConfigFile IO (Maybe T.Text)
replaceIfNew item itemId = do 
  old <-  lift $ Strict.readFile "itemid" `catch` readHandler
  if itemId == old
     then return Nothing
     else do lift $ writeFile "itemid" itemId `catch` writeHandler
             replaceFeedItem' item

getFirstFeedItem :: BL.ByteString -> Maybe Item
getFirstFeedItem rssStr = do feed <- parseFeedString . decode $ BL.unpack rssStr
                             headMay . getFeedItems $ feed

replaceFeedItem' :: Item -> ReaderT ConfigFile IO (Maybe T.Text)
replaceFeedItem' item =
  replaceFeedItem item <$>
  asks absPath <*>
  asks tags
