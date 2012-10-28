#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow
import Data.Monoid

import Hakyll
import Text.Pandoc

main :: IO ()
main = hakyllWith config $ do
         match "index.html" $ do
           route idRoute
           compile copyFileCompiler
         match "js/**" $ do
           route idRoute
           compile copyFileCompiler
         match "*.gexf" $ do
           route idRoute
           compile copyFileCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
         { deployCommand =
               "./hakyll.hs rebuild && \
               \cp -r _site/ ~/Dropbox/www/tanakh.jp/"
         }
