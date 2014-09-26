module Config (ConfigFile(..), readConfigFile) where
import Control.Applicative
import Control.Monad.Except
import Data.ConfigFile

data ConfigFile = ConfigFile { rssURL :: String
                             , absPath :: String
                             , podURL :: String
                             , username :: String
                             , password :: String
                             , aspect :: String
                             , tags :: [String] } deriving Show

readConfigFile :: FilePath -> IO (Either CPError ConfigFile)
readConfigFile confPath =
  runExceptT $
  do cp <- join $ liftIO $
           readfile emptyCP confPath
     ConfigFile <$>
       get cp "RSS" "url" <*>
       get cp "RSS" "abspath" <*>
       get cp "Diaspora" "url" <*>
       get cp "Diaspora" "username" <*>
       get cp "Diaspora" "password" <*>
       get cp "Diaspora" "aspect" <*>
       (words <$> get cp "Diaspora" "tags")
