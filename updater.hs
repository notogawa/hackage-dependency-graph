{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding ( writeFile )
import Control.Applicative
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Map as M
import qualified Data.Text as T
import Text.GEXF
import Text.XML
import Data.List ( intercalate, nub, sort )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Distribution.Hackage.DB ( readHackage )

type PackageVersionGraph = G.Gr PackageIdentifier ()

type PackageGraph = G.Gr PackageName ()

showPackage :: PackageIdentifier -> String
showPackage (PackageIdentifier name version) =
    showPackageName name ++ '-' : intercalate "." (map show $ versionBranch version)

showPackageName :: PackageName -> String
showPackageName (PackageName name) = name

hackageDir :: FilePath
hackageDir = "hackage"

main :: IO ()
main = packageGraph

allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies desc = libdeps ++ exedeps
    where
      libdeps = maybe [] condTreeConstraints $ condLibrary desc
      exedeps = condExecutables desc >>= condTreeConstraints . snd

packageGraph = do
  hackage <- M.mapKeys PackageName . M.delete "base" <$> readHackage
  let nodes = zip [0..] $ M.keys hackage
      package2node = M.fromList $ map (uncurry (flip (,))) nodes
      edges = nub . sort . concat $ do
                 (name, versions) <- M.toList hackage
                 let Just src = M.lookup name package2node
                 return [ (src, dst, ())
                        | dependency <- M.toList versions >>= allDependencies . snd
                        , (_, p) <- searchPackage dependency hackage
                        , let Just dst = M.lookup (pkgName' p) package2node
                        ]
      graph = G.mkGraph nodes edges :: PackageGraph
  writeFile def "hdg/hackage.gexf" $ toDocument (T.pack . showPackageName) graph

searchPackage (Dependency packageName versionRange) hackage =
    case M.lookup packageName hackage of
      Nothing -> []
      Just versions -> filter (matchPackage packageName versionRange) $ M.toList versions

matchPackage packageName versionRange (version, package) =
    packageName == pkgName' package &&
    withinRange (pkgVersion' package) versionRange

pkgName' = pkgName . package . packageDescription

pkgVersion' = pkgVersion . package . packageDescription
