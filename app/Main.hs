{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS
import qualified Data.Text as T
import Data.Aeson as J
import Data.Aeson.Types as J
import System.Environment

sendResponse :: Monad m => (Response -> m b) -> ExceptT BL.ByteString m b -> m b
sendResponse send m = runExceptT m >>= \case
  Left e -> send $ responseLBS status400 [] e
  Right a -> return a

commentArtifact :: Object -> Parser (IO HC.Request)
commentArtifact obj = do
  buildUrl <- obj .: "CIRCLE_BUILD_URL"
  owner <- obj .: "CIRCLE_PROJECT_USERNAME"
  repo <- obj .: "CIRCLE_PROJECT_REPONAME"
  sha <- obj .: "CIRCLE_SHA1"
  return $ do
    token <- getEnv "GITHUB_TOKEN"
    initialRequest <- HC.parseRequest
      $ T.unpack $ T.intercalate "/" ["https://api.github.com/repos", owner, repo, "commits", sha, "comments"]
    return initialRequest
      { HC.method = "POST"
      , HC.requestBody = HC.RequestBodyLBS $ J.encode $ object
        [ "body" .= T.intercalate "/"
          [ buildUrl, "artifacts", "0"
          , "generated-site/index.html"]
        -- TODO: , "path" .=
        ]
      , HC.requestHeaders =
        [ ("Authorization", B.pack token)
        , ("User-Agent", "haskell-jp-blog-artifact")
        ]
      }

main :: IO ()
main = do
  man <- HC.newManager tlsManagerSettings
  runEnv 8899 $ \req send -> sendResponse send $ do
    body <- liftIO $ strictRequestBody req
    obj <- case decode body of
      Nothing -> throwE "Malformed request"
      Just a -> pure a
    case parse commentArtifact obj of
      J.Success mkReq -> liftIO $ do
        req' <- mkReq
        resp <- HC.httpLbs req' man
        send $ responseLBS (HC.responseStatus resp) [] $ HC.responseBody resp
      J.Error e -> throwE $ BL.pack e
