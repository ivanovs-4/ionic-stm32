{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IvoryMain where


import Control.Monad (when)
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language
import Ivory.Language (Module())
import Ivory.Stdlib (stdlibModules)
import Ivory.Stdlib.String

import qualified Ivored.Main as Main


compileIvory :: FilePath -> IO ()
compileIvory dirpath = do
  let opts = initialOpts { outDir = Just dirpath, srcLocs = True }
  runCompiler modules stdlibStringArtifacts opts

modules :: [Module]
modules = [ Main.cmodule
          ]
          <> stdlibModules
