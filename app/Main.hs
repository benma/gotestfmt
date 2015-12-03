{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Monoid ((<>))
import System.Directory (doesDirectoryExist)
import System.IO (stdout)
import Data.List.Split (splitOn)
import System.Environment (lookupEnv)
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Text.Megaparsec as P
import Text.Megaparsec.Prim (MonadParsec)
import qualified Control.Monad as M
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Tardis as Tardis

type Output = BSB.Builder
type Parser a = forall s. (P.Stream s Char) => P.ParsecT s (Tardis.TardisT String Output IO) a

main :: IO ()
main = do
  gopath <- maybe (error "GOPATH not defined") (splitOn (":")) <$> lookupEnv "GOPATH"
  stdin <- BS.getContents
  (_, output) <- flip Tardis.execTardisT ("", mempty) $ P.runParserT (gotestformat gopath) "" stdin
  BSB.hPutBuilder stdout output

absPath :: [String] -> String -> IO String
absPath gopath gopackage = do
  let paths = map (++ "/src/" ++ gopackage) gopath
  M.filterM doesDirectoryExist paths >>= \case
    [x] -> return x
    _ -> return gopackage

parsePath :: MonadParsec s m Char => m String
parsePath = P.manyTill P.anyChar (P.lookAhead P.spaceChar)

data Part = Location String | FailLine String

gotestformat :: [String] -> Parser ()
gotestformat gopath = do
  M.void $ many $ do
    found <- P.try $ skipTo $ P.choice
      [ do
          P.try $ M.void $ P.string ("\tLocation:\t"::String)
          Location <$> parsePath
      , do
          P.try $ M.void $ P.string "FAIL\t"
          FailLine <$> parsePath
      ]
    case found of 
      Location filename -> do
        gopackage <- lift $ Tardis.getFuture
        write $ concat [ gopackage
                       , "/"
                       , filename
                       , ":1"]
      FailLine line -> do
        write $ concat ["FAIL:\t", line, "\n"]
        path <- lift . lift $ absPath gopath line
        lift $ Tardis.sendPast path
  skipTo P.eof

write :: String -> Parser ()
write = writeb . BSB.string8

writeb :: BSB.Builder -> Parser ()
writeb b = lift $ Tardis.modifyForwards (<> b)

skipTo :: Parser b -> Parser b
skipTo p = do
  (part, end) <- manyTill' (BSB.char8 <$> P.anyChar) p
  writeb part
  return end

manyTill' :: (MonadParsec s m t, Monoid b) => m b -> m end -> m (b, end)
manyTill' p end = P.choice [ (mempty,) <$> end
                           , do
                               x <- p
                               (xs, endVal) <- manyTill' p end
                               return (x <> xs, endVal)
                           ]

