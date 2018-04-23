
module Main where

import Prelude

import Bridge.Instructions
import Bridge.Types

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as BS
import Data.Function (on)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.PureScript.Bridge
import System.Directory
import System.FilePath
import System.Environment
import System.Exit (die)

main :: IO ()
main = do
    let bridge = defaultBridge <|> nonEmptyBridge <|> mapBridge
    writePSTypes' (buildBridge bridge) bridgeTypes

emptyModule :: ModuleName -> Modules' -> Modules'
emptyModule name modules = case M.lookup name' modules of
    Nothing -> M.insert name' (newitem, manualInstructions) modules
    Just _ -> modules
    where
    newitem = PSModule {
        psModuleName = name',
        psImportLines = M.empty,
        psTypes = []
    }
    name' = T.pack name

writePSTypes' :: FullBridge -> [(SumType 'Haskell, Instructions 'PureScript)] -> IO ()
writePSTypes' br sts = do
    let bridged = map (first (bridgeSumType br)) sts
    let modules = foldr sumTypeToModule' M.empty bridged
    mapM_ printModule' $ M.elems modules
    T.putStrLn "Successfully created your PureScript modules.\n"
    T.putStrLn "Make sure you have the following PureScript packages installed:\n"
    let packages = sumTypesToNeededPackages (fmap fst bridged)
    mapM_ (T.putStrLn . mappend "  - ") packages

type Modules' = M.Map Text (PSModule, Instructions 'PureScript)

sumTypeToModule' :: (SumType 'PureScript, Instructions 'PureScript) -> Modules' -> Modules'
sumTypeToModule' (st@(SumType t _), d) = M.alter (Just . updateModule) (_typeModule t)
  where
    updateModule Nothing = (PSModule {
          psModuleName = _typeModule t
        , psImportLines = dropSelf $ typesToImportLines M.empty (getUsedTypes st)
        , psTypes = [st]
        }, d)
    updateModule (Just (m, d1)) = (m {
        psImportLines = dropSelf $ typesToImportLines (psImportLines m) (getUsedTypes st)
      , psTypes = st : psTypes m
      }, combineInstructions d d1)
    dropSelf = M.delete (_typeModule t)

printModule' :: (PSModule, Instructions 'PureScript) -> IO ()
printModule' psm@(m, _) = do
  [resultDir] <- getArgs
  let mPath = resultDir </> mFile
  let mDir = takeDirectory mPath
  unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
  T.writeFile mPath (moduleToText m)
  patchModule mFile psm
  where
    mFile = (joinPath . map T.unpack . T.splitOn "." $ psModuleName m) <> ".purs"

-- Like `lookup` but returns a list that can be empty or with several items
multiLookup :: Eq k => k -> [(k, v)] -> [v]
multiLookup k l = map snd $ filter (\x -> fst x == k) l

patchModule :: FilePath -> (PSModule, Instructions 'PureScript) -> IO ()
patchModule destFile (m, instructions) = do
  [resultDir] <- getArgs
  let base = "patch" </> destFile
  customExports <-
    genCustoms (base <> ".export") instructions genExports
  customImports <-
    genCustoms (base <> ".import") instructions genImports
  customAppends <-
    genCustoms (base <> ".append") instructions genAppends
  allLines <- lines <$> (readFile $ resultDir </> destFile)
  forceList allLines  -- Force reading the file, because we are going to overwrite it later
                      -- and we need the handle to be closed
  let (beforeModule, moduleLine, afterModule) = splitOn (startsWith "module") allLines
  let moduleName = getModuleName moduleLine
  let patchedLines = beforeModule ++
                     ["module " ++ moduleName] ++
                     [customExports] ++
                     ["where"] ++
                     [customImports] ++
                     afterModule ++
                     [customAppends]
  let patched = unlines patchedLines
  writeFile (resultDir </> destFile) patched
  where
    splitOn :: (a -> Bool) -> [a] -> ([a], a, [a])
    splitOn pred xs = (before, head after, tail after)
      where
        (before, after) = break pred xs
    getModuleName :: String -> String
    getModuleName l = (words l) !! 1
    startsWith :: String -> String -> Bool
    startsWith prefix str | length str < length prefix = False
    startsWith prefix str = all (\(x, y) -> x == y) $ zip prefix str
    forceList :: [a] -> IO ()
    forceList [] = return ()
    forceList (x:xs) = forceList xs

genCustoms
  :: FilePath
  -> Instructions language
  -> (Instructions language -> String)
  -> IO String
genCustoms file instructions gen = do
  fileExists <- doesFileExist file
  fileStr <- if fileExists then readFile file else pure ""
  pure $ (gen instructions) <> "\n" <> fileStr

--
-- Custom bridge parts
--

nonEmptyBridge :: BridgePart
nonEmptyBridge = do
  typeName ^== "NonEmpty"
  typeModule ^== "Data.List.NonEmpty"
  psArray

psArray :: MonadReader BridgeData m => m PSType
psArray = TypeInfo "purescript-prim" "Prim" "Array" <$> psTypeParameters

mapBridge :: BridgePart
mapBridge = do
  typeName ^== "Map"
  typeModule ^== "Data.Map.Base"
  psMap

psMap :: MonadReader BridgeData m => m PSType
psMap = TypeInfo "purescript-maps" "Data.Map" "Map" <$> psTypeParameters
