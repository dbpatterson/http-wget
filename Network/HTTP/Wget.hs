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
    ) where

import System.Process
import System.Exit
import System.IO
import Numeric (showHex)
import Data.List (intercalate)

-- | Get a response from the given URL with the given parameters.
wget :: Monad m
     => String -- ^ The URL.
     -> [(String, String)] -- ^ Get parameters.
     -> [(String, String)] -- ^ Post parameters. If empty, this will be a get request.
     -> IO (m String) -- ^ The response body.
wget url get post = do
    let getSepChar :: Char
        getSepChar = if '?' `elem` url then '&' else '?'
        get' :: String
        get' = if null get then "" else getSepChar : urlEncodePairs get
        post' :: [String]
        post' = if null post
                    then []
                    else ["--post-data", urlEncodePairs post]
    (Nothing, Just hout, Just herr, phandle) <- createProcess $ (proc "wget"
            ((url ++ get') : post' ++ ["-O", "-"])
        ) { std_out = CreatePipe, std_err = CreatePipe }
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> hGetContents hout >>= return . return
        _ -> hGetContents herr >>= return . fail

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
