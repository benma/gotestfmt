{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Monad as M
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.Split (splitOn)
import Lens.Family (view)
import qualified Pipes as P
import qualified Pipes.Group as PG
import qualified Pipes.ByteString as PBS
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import qualified Text.Regex.PCRE.Heavy as RE

main :: IO ()
main = do
    gopath <- maybe (error "GOPATH not defined") (splitOn (":")) <$> lookupEnv "GOPATH"
    P.runEffect $ produceLines PBS.stdin P.>-> handleLine gopath P.>-> PBS.stdout

produceLines :: (Monad m) => P.Producer BS.ByteString m r -> P.Producer BS.ByteString m r
produceLines = PG.folds BS.append BS.empty (`BS.append` "\n") . view PBS.lines

absPath :: [String] -> String -> IO String
absPath gopath gopackage = do
  let paths = map (++ "/src/" ++ gopackage) gopath
  M.filterM doesDirectoryExist paths >>= \case
    [x] -> return x
    _ -> return gopackage

handleLine :: [String] -> P.Pipe BS.ByteString BS.ByteString IO ()
handleLine gopath = M.forever $ do
    line <- P.await
    if BS.isInfixOf "Location:" line
        then do
            (restLines, scanResult) <- takeTill $ maybeRe [RE.re|FAIL\s(.+?)\s|]
            case scanResult of
                [(_,[gopackage'])] -> do
                    abspath <- lift $ absPath gopath $ BSC.unpack gopackage'
                    let rewriteLocation =
                            RE.sub
                            [RE.re|\tLocation:\s(.+)|]
                            (\[relpath] -> concat [abspath, "/", relpath, ":1"])
                    M.forM_ (line:restLines) (P.yield . rewriteLocation)        
                _ -> do
                    P.yield "(gotestfmt: this section could not be read)\n"
                    M.forM_ (line:restLines) P.yield
        else P.yield line
  where
    maybeRe r s = case RE.scan r s of
        [] -> Nothing
        r' -> Just r'

takeTill :: (Monad m) => (el -> Maybe r) -> P.Consumer' el m ([el], r)
takeTill p = go []
  where
    go els = do
        el <- P.await
        case p el of
            Just r -> return (reverse (el:els), r)
            _ -> go (el:els)
