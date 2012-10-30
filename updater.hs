{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Prelude hiding ( writeFile )
import Control.Applicative
import Control.Monad
import System.Directory
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Map as M
import qualified Data.Text as T
import Text.GEXF
import Text.XML
import Data.List ( intercalate, nub, sort, groupBy )
import Data.Function ( on )
import Cabal
import Distribution.Package
import Distribution.Version

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

packageGraph = do
  packages <- sort <$> getPackageDirs >>= getPackages >>= mapM getPackageInfo
  let nodes = zip [0..] $ nub $ map (pkgName . fst) packages
      package2node = M.fromList $ map (uncurry (flip (,))) nodes
      pkgName2packages =
          M.fromList [ (pkgName $ fst $ head ps, ps)
                     | ps <- groupBy ((==) `on` (pkgName . fst)) packages
                     ]
      edges = nub . sort . concat $ do
                 (package, dependencies) <- packages
                 let Just src = M.lookup (pkgName package) package2node
                 return [ (src, dst, ())
                        | dependency <- dependencies
                        , (p, _) <- searchPackage dependency pkgName2packages
                        , let Just dst = M.lookup (pkgName p) package2node
                        ]
      graph = G.mkGraph nodes edges :: PackageGraph
  writeFile def "hackage.gexf" $ toDocument (T.pack . showPackageName) graph

packageVersionGraph = do
  packages <- getPackageDirs >>= getPackages >>= mapM getPackageInfo
  let nodes = zip [0..] $ map fst packages
      package2node = M.fromList $ map (uncurry (flip (,))) nodes
      pkgName2packages =
          M.fromList [ (pkgName $ fst $ head ps, ps)
                     | ps <- groupBy ((==) `on` (pkgName . fst)) packages
                     ]
      edges = concat $ do
                 (package, dependencies) <- packages
                 let Just src = M.lookup package package2node
                 return [ (src, dst, ())
                        | dependency <- dependencies
                        , (p, _) <- searchPackage dependency pkgName2packages
                        , let Just dst = M.lookup p package2node
                        ]
      graph = G.mkGraph nodes edges :: PackageVersionGraph
  writeFile def "hackage.full.gexf" $ toDocument (T.pack . showPackage) graph

getDirectoryContents' path =
    filter (("." /=) . take 1) <$> getDirectoryContents path

getPackageDirs =
    getDirectoryContents' hackageDir >>=
    filterM (doesDirectoryExist . (hackageDir ++) . ("/" ++)) . filter ("base" /=)

getPackages packageDirs =
    concat <$> (forM packageDirs $ \packageDir -> do
                  versionDirs <- getDirectoryContents' (hackageDir ++ "/"++ packageDir)
                  return $ map ((,) packageDir) versionDirs)

getPackageInfo (packageDir, versionDir) =
    parseCabalFile (hackageDir ++ "/"++packageDir++"/"++versionDir++"/"++packageDir++".cabal")

searchPackage (Dependency packageName versionRange) pkgName2packages =
    case M.lookup packageName pkgName2packages of
      Nothing -> []
      Just packages -> filter (matchPackage packageName versionRange) packages

matchPackage packageName versionRange (package, _deps) =
    packageName == pkgName package &&
    withinRange (pkgVersion package) versionRange
