{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative
import Control.Monad
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language
import Ivory.Language.Ion.Code
import Ivory.Stdlib (stdlibModules)
import Ivory.Stdlib.String
import Options.Applicative as OA
import Text.Pretty.Simple

import qualified Data.Text.Lazy as TL

import Ivored.MainIvored
import Pilot
import Schedule


data Ops = Ops {
    targetDir :: FilePath
  }
  deriving Show

parseOps :: Parser Ops
parseOps = do
  targetDir <- argument str $ metavar "TARGET_DIR"
  pure $ Ops{..}

main :: IO ()
main = join $ customExecParser (prefs showHelpOnError) $ info (opts <**> helper)
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
    compileCopiloted pilotInfo targetDir
    compileIonicSchedule scheduleParams targetDir
    compileIvoryMain scheduleParams targetDir

  where

    compileIonicSchedule :: ScheduleParams -> String -> IO ()
    compileIonicSchedule sp targetDir = do
        let ivoryOpts = initialOpts { scErrors = False
                                    , srcLocs = True
                                    , outDir = Just targetDir
                                    }
        ionCompile ivoryOpts (sched_name sp) (ionSchedule sp)

    compileIvoryMain :: ScheduleParams -> FilePath -> IO ()
    compileIvoryMain sp dirpath = do
        let opts = initialOpts { outDir = Just dirpath, srcLocs = True }
        runCompiler (modules sp) stdlibStringArtifacts opts

    modules :: ScheduleParams -> [Module]
    modules sp = mempty
              <> [ cmodule sp ]
              <> stdlibModules
