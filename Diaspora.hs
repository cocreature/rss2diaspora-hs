{-# LANGUAGE OverloadedStrings #-}
module Diaspora (post) where
import Control.Applicative
import Config
import Data.Aeson
import Control.Monad.Reader
import Text.Regex.Posix
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Network.HTTP.Conduit

data StatusMessage = StatusMessage !T.Text deriving (Show)
data PostData = PostData StatusMessage !String deriving (Show)

instance ToJSON StatusMessage where
  toJSON (StatusMessage text) = object [ "text" .= text ]
instance ToJSON PostData where
  toJSON (PostData status aspects) =
    object ["status_message" .= status,"aspect_ids" .= aspects]

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

getToken :: CookieJar -> ReaderT ConfigFile IO (BL.ByteString, CookieJar)
getToken cookies =
  withManager $
  \manager ->
    do request' <- join $ lift . parseUrl <$> asks podURL
       let request =
             request' {cookieJar = Just cookies}
           regex =
             "content=\"(.*?)\"\\s+name=\"csrf-token" :: BL.ByteString
       res <- httpLbs request manager
       let (_,_,_,groups) =
             responseBody res =~ regex :: (BL.ByteString,BL.ByteString,BL.ByteString,[BL.ByteString])
       return (head groups,responseCookieJar res)

login :: CookieJar -> BL.ByteString -> ReaderT ConfigFile IO CookieJar
login cookies token =
  do username' <- asks username
     password' <- asks password
     podurl' <- asks podURL
     request' <- lift $
                 parseUrl (podurl' ++ "/users/sign_in")
     let request =
           urlEncodedBody
             [("user[username]",BC.pack username')
             ,("user[password]",BC.pack password')
             ,("authenticity_token",toStrict token)] $
           request' {cookieJar = Just cookies}
     withManager $
       \manager ->
         do res <- httpLbs request manager
            return $ responseCookieJar res

post' :: CookieJar -> BL.ByteString -> T.Text -> ReaderT ConfigFile IO BL.ByteString
post' cookies token text =
  do podurl' <- asks podURL
     request' <- lift $
                 parseUrl (podurl' ++ "/status_messages")
     body <- encode <$>
             PostData (StatusMessage text) <$>
             asks aspect
     let request =
           request' {method = "POST"
                    ,cookieJar = Just cookies
                    ,requestBody =
                       RequestBodyLBS body
                    ,requestHeaders =
                       [("content-type","application/json")
                       ,("accept","application/json")
                       ,("x-csrf-token",toStrict token)]}
     withManager $
       \manager ->
         do res <- httpLbs request manager
            return $ responseBody res

post ::  T.Text -> ReaderT ConfigFile IO BL.ByteString
post text =
  do (token,cj) <- getToken $
                   createCookieJar []
     cj' <- login cj token
     (token',cj'') <- getToken cj'
     post' cj'' token' text
