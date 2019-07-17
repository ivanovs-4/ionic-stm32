{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Options.Applicative as OA
import qualified Data.Text.Lazy as TL
import Text.Pretty.Simple

import           Ivory.Compile.C.CmdlineFrontend

import           Ivory.Language.Ion.Code


import BlinkIon


data Ops = Ops {
    targetDir :: FilePath
  }
  deriving Show

parseOps :: Parser Ops
parseOps = do
  targetDir <- argument str (
                metavar "TARGET_DIR"
             <> OA.help "Target dir" )
  pure $ Ops{..}

main :: IO ()
main = join $ execParser $ info (opts <**> helper)
        ( fullDesc
       <> progDesc "Compile ion program and generate *.c files"
       <> header "Blink ion"
        )
  where
    opts :: Parser (IO ())
    opts = (compileMain <=< ppAndPass) <$> parseOps

    ppAndPass :: (Show a) => a -> IO a
    ppAndPass = (\f a -> f a >> pure a) $ putStrLn'stderr . TL.unpack . pShow

    putStrLn'stderr :: String -> IO ()
    putStrLn'stderr s = hPutStr stderr s >> hPutStr stderr "\n"

compileMain :: Ops -> IO ()
compileMain Ops{..} = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Just targetDir
                              }
  ionCompile ivoryOpts "main" simpleSchedule
  -- ionCompile ivoryOpts "timer" exampleTimer
  -- ionCompile ivoryOpts "exampleChain" exampleChain
  -- ionCompile ivoryOpts "giant_ugly_test" test
  pure ()
