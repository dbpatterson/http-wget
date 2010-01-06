{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------
-- |
-- Module        : Network.HTTP.Wget
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Provide a simple HTTP client interface by wrapping the wget command line tool.
--
---------------------------------------------------------
module Network.HTTP.Wget
    ( wget
    , wget'
    , WgetException (..)
    ) where

import System.Process
import System.Exit
import System.IO
import Numeric (showHex)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad.Trans
import Control.Failure
import Control.Exception
import Data.Generics
import Data.Char (isSpace)
import Data.Text (strip)

newtype WgetException = WgetException String
    deriving (Show, Typeable)
instance Exception WgetException

-- | Get a response from the given URL with the given parameters.
wget :: (MonadIO m, MonadFailure WgetException m)
     => String -- ^ The URL.
     -> [(String, String)] -- ^ Get parameters.
     -> [(String, String)] -- ^ Post parameters. If empty, this will be a get request.
     -> m String -- ^ The response body.
wget url get post = wget' url get post >>= return.snd

-- | Get a response from the given URL with the given parameters, including headers.
wget' :: (MonadIO m, MonadFailure WgetException m)
     => String -- ^ The URL.
     -> [(String, String)] -- ^ Get parameters.
     -> [(String, String)] -- ^ Post parameters. If empty, this will be a get request.
     -> m ([(String, String)], String) -- ^ The headers and response body.
wget' url get post = do
    let getSepChar :: Char
        getSepChar = if '?' `elem` url then '&' else '?'
        get' :: String
        get' = if null get then "" else getSepChar : urlEncodePairs get
        post' :: [String]
        post' = if null post
                    then []
                    else ["--post-data", urlEncodePairs post]
    (Nothing, Just hout, Just herr, phandle) <- liftIO $
        createProcess $ (proc "wget"
            ((url ++ get') : post' ++ ["-O", "-", "--save-headers"])
        ) { std_out = CreatePipe, std_err = CreatePipe }
    exitCode <- liftIO $ waitForProcess phandle
    case exitCode of
        ExitSuccess -> liftIO $ hGetContents hout >>= return . parseHeaders
        _ -> liftIO (hGetContents herr) >>= failure . WgetException

parseHeaders :: String -> ([(String, String)], String)
parseHeaders resp = parseHeaders' (drop 1 (lines resp)) []

parseHeaders' :: [String] -> [(String, String)] -> ([(String, String)], String)
parseHeaders' (s:ss) hs = if all isSpace s
                            then (reverse hs, unlines ss) 
                            -- not sure if http spec allows header values to have colons in them, but incase yes.
                            else parseHeaders' ss ((head lt, (reverse.(dropWhile isSpace).reverse) (intercalate ": " (tail lt))):hs)
    where lt = splitOn ": " s
parseHeaders' [] hs = error "parseHeaders' failure: empty string list"

urlEncodePairs :: [(String, String)] -> String
urlEncodePairs = intercalate "&" . map urlEncodePair

urlEncodePair :: (String, String) -> String
urlEncodePair (x, y) = urlEncode x ++ '=' : urlEncode y

urlEncode :: String -> String
urlEncode = concatMap urlEncodeChar

urlEncodeChar :: Char -> String
urlEncodeChar x
    | safeChar (fromEnum x) = return x
    | otherwise = '%' : showHex (fromEnum x) ""

safeChar :: Int -> Bool
safeChar x
    | x >= fromEnum 'a' && x <= fromEnum 'z' = True
    | x >= fromEnum 'A' && x <= fromEnum 'Z' = True
    | x >= fromEnum '0' && x <= fromEnum '9' = True
    | otherwise = False
