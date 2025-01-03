module Input (
  getInput,
) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (IOException, catch, throwIO)
import Control.Lens
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Lens (packedChars, unpackedChars)
import Network.HTTP.Simple (
  Request,
  RequestHeaders,
  getResponseBody,
  getResponseStatusCode,
  httpBS,
  parseRequest_,
  setRequestHeaders,
  setRequestMethod,
 )
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

getInput :: Int -> IO String
getInput (show -> day) = do
  let path = "inputs/" <> day
  file <- safeRead path
  case file of
    Nothing -> do
      input <- fetchInput day
      BS.writeFile path input
      input ^. unpackedChars . to pure
    Just input -> pure input

createHeaders :: IO RequestHeaders
createHeaders = do
  session <- view packedChars <$> getEnv "SESSION"
  pure [("cookie", "session=" <> session)]

makeRequest :: String -> IO Request
makeRequest day = do
  let url = "https://adventofcode.com/2024/day/" <> day <> "/input"
  print url
  headers <- createHeaders
  pure $
    parseRequest_ url
      & setRequestMethod "GET"
      & setRequestHeaders headers

safeRead :: String -> IO (Maybe String)
safeRead path = (Just <$> readFile path) `catch` handleExists
 where
  handleExists :: IOException -> IO (Maybe String)
  handleExists e
    | isDoesNotExistError e = return Nothing
    | otherwise = throwIO e

fetchInput :: String -> IO BS.ByteString
fetchInput day = do
  putStrLn $ "Fetching day : " <> day <> " ....."
  void $ loadFile defaultConfig
  req <- makeRequest day
  resp <- httpBS req
  case getResponseStatusCode resp of
    200 -> pure $ getResponseBody resp
    _ -> do
      putStrLn $ "Could not fetch input for day " <> day
      error $ show resp
