{-# LANGUAGE OverloadedStrings #-}
import Hakyll

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
