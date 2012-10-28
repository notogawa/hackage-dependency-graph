module Cabal
    (
      parseCabalFile
    ) where

import Control.Applicative
import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Verbosity ( silent )

parseCabalFile :: FilePath -> IO (PackageIdentifier, [Dependency])
parseCabalFile file = do
  cabal <- readPackageDescription silent file
  let pid = package $ packageDescription cabal
  return . (,) pid . map simplifyDependency . fromJust $ fromLibrary cabal <|> fromExecutable cabal
    where
      fromLibrary c     = condTreeConstraints <$> condLibrary c
      fromExecutable c  = condTreeConstraints . snd <$> listToMaybe (condExecutables c)
