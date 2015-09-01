{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import Control.Monad.Logger
import Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
import Data.Maybe
import Data.Text (Text)

import Text.Hamlet
import Text.Jasmine

import Yesod
import Yesod.Default.Util
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Yesod.Static

import PSS.Database
import PSS.Foundation
import PSS.Operations

{-|
Module: Main
Description: Doing I\/O-Stuff
Copyright: Julius Quasebarth, Robin Hankel 2015
License: GPL-3
Maintainer: julius@quasebarth.de
Stability: experimental
Portability: POSIX

This module mainly serves the webpage backend.
-}

main :: IO ()
main = runNoLoggingT $ withMySQLPool connInfo 10 $
    \pool -> NoLoggingT $ do
        runSqlPool (runMigration migrateAll) pool
        st <- static "static"
        warp 3001 $ App pool st
