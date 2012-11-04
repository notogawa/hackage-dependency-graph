{-# LANGUAGE OverloadedStrings #-}
import Hakyll

main :: IO ()
main = hakyllWith config $ do
         match "hdg/index.html" $ do
           route idRoute
           compile copyFileCompiler
         match "hdg/js/**" $ do
           route idRoute
           compile copyFileCompiler
         match "hdg/*.gexf" $ do
           route idRoute
           compile copyFileCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
         { deployCommand = "scp -r _site www.notogawa.com:"
         }
