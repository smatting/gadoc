{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Gadoc.Lib

where

-- base
import Foreign.Ptr
import Data.Maybe
import Data.List (findIndex, splitAt, isPrefixOf, tails)
import Control.Monad.State
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as C8
import Data.Char (isSpace)
import Data.Ord
import Data.Foldable
import System.Environment (lookupEnv)
import System.IO (stderr, hPutStr)
import System.Exit (exitFailure)

-- extra
import Data.List.Extra (sortBy)

-- bytestring
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS

-- vector
import qualified Data.Vector.Storable as V
import qualified Data.Vector as VV

-- text
import qualified Data.Text.Lazy as TL hiding (foldl')
import qualified Data.Text.Lazy.IO as TL hiding (putStrLn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

-- hoogle
import General.Str
import Hoogle (hoogle, Target(..))
import Output.Items (listItems, lookupItem)
import Input.Item
import General.Util
import General.Store
import General.Str (bstr0Split, bstrUnpack)
import General.Store
import General.Util (unHTML)
import Input.Cabal (readGhcPkg)
import Input.Settings (loadSettings)

-- aeson
import Data.Aeson.Text
import Data.Aeson

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- filepath
import System.FilePath.Posix ((</>))

-- easy-file
import System.EasyFile (createDirectoryIfMissing, doesFileExist, takeFileName)

-- directory
import System.Directory (canonicalizePath, copyFile, doesFileExist, findFile,
                        getModificationTime, getDirectoryContents, doesDirectoryExist, listDirectory,
                        findExecutable, getFileSize)

-- gadoc
import Paths_gadoc (getDataFileName)

-- open-browser
import Web.Browser (openBrowser)

-- filemanip (find)
import qualified System.FilePath.Find as Find

-- process
import System.Process (shell, readCreateProcess, CreateProcess(..))

-- regex-tdfa
import Text.Regex.TDFA ((=~))



data NamesSize a where NamesSize :: NamesSize Int deriving Typeable
data NamesItems a where NamesItems :: NamesItems (V.Vector TargetId) deriving Typeable
data NamesText a where NamesText :: NamesText BS.ByteString deriving Typeable

newtype UrlWithoutFragment = UrlWithoutFragment String
  deriving (Eq, Ord, Show)

instance ToJSON UrlWithoutFragment
  where
    toJSON (UrlWithoutFragment s) = String (T.pack s)

data NormalizedUrl = NormalizedUrl UrlWithoutFragment (Maybe String)
  deriving (Eq, Ord, Show)

newtype Ref = Ref Int
  deriving (Eq, Ord, Show)

data RefUrl = RefUrl Ref (Maybe String)
  deriving (Eq, Ord, Show)

newtype PackageName = PackageName String
  deriving (Eq, Ord, Show)

instance ToJSON PackageName
  where
    toJSON (PackageName s) = String (T.pack s)

data ModuleName = ModuleName PackageName String
  deriving (Eq, Ord, Show)

instance ToJSON ModuleName
  where
    toJSON (ModuleName pn mn) =
      object ["package" .= pn, "module" .= mn]

data TargetType = PackageType | ModuleType | NoneType
  deriving (Eq, Ord, Show)

data CompressedTarget =
  CompressedTarget
    {
      nTargetURL :: RefUrl,
      nTargetPackage :: Maybe (Ref, RefUrl),
      nTargetModule :: Maybe (Ref, RefUrl),
      nTargetType :: TargetType,
      nTargetItem :: String,
      nTargetDocs :: String
    }
    deriving (Eq, Ord, Show)

instance ToJSON RefUrl
  where
    toJSON (RefUrl ref mf) =
      object ([ "i" .= toJSON ref ] <> maybe [] (\f -> ["f" .= String (T.pack f)] ) mf)

instance ToJSON Ref
  where
    toJSON (Ref i) = Number (fromIntegral i)

instance ToJSON TargetType
  where
    toJSON NoneType = Number 0
    toJSON ModuleType = Number 1
    toJSON PackageType = Number 2

instance ToJSON CompressedTarget where
  toJSON ct =
    object
      ( [ "u" .= toJSON (nTargetURL ct),
          "t" .= toJSON (nTargetType ct),
          "i" .= String (T.pack (nTargetItem ct))
        ]
          <> maybe mempty (\(r, ru) -> ["p" .= object [ "r" .= toJSON r, "u" .= toJSON ru ] ]) (nTargetPackage ct)
          <> maybe mempty (\(r, ru) -> ["m" .= object [ "r" .= toJSON r, "u" .= toJSON ru ] ]) (nTargetModule ct)
      )

data Enumeration a =
  Enumeration (Map a Int) Int
  deriving (Show, Eq, Ord)

emptyEnum = Enumeration M.empty 0

data DocState
  = DocState
      { _urlsEnum :: Enumeration UrlWithoutFragment,
        _packageEnum :: Enumeration PackageName,
        _moduleEnum :: Enumeration ModuleName,
        _docTitle :: String
      }
  deriving (Show, Eq, Ord)

newDocState title =
  DocState
    { _urlsEnum = emptyEnum,
      _packageEnum = emptyEnum,
      _moduleEnum = emptyEnum,
      _docTitle = title
    }

instance ToJSON DocState
  where
    toJSON ds =
      object [ "urls" .= listEnumeration (_urlsEnum ds),
               "packages" .= listEnumeration (_packageEnum ds),
               "modules" .= listEnumeration (_moduleEnum ds),
               "docTitle" .= String (T.pack (_docTitle ds))
             ]

data PackageDocs =
  PackageDocs
    { _contentsUrl :: Maybe Text,
      _modules :: [(Text, Text)]
    }
  deriving (Show, Eq, Ord)

instance ToJSON PackageDocs
  where
    toJSON (PackageDocs url modules) =
      object [ "url" .= url,
               "modules" .= (fmap (\(n, u) -> object ["name" .= n, "url" .= u]) modules)
             ]

enum :: Ord a => a -> Enumeration a -> (Enumeration a, Ref)
enum obj e@(Enumeration m nextIdx) =
  case M.lookup obj m of
    Nothing -> (Enumeration (M.insert obj nextIdx m) (nextIdx + 1), Ref nextIdx)
    Just idx -> (e, Ref idx)

listEnumeration :: Enumeration a -> [a]
listEnumeration (Enumeration m _) =
  fst <$> sortBy (comparing snd) (M.toList m)

instance ToJSON a => ToJSON (Enumeration a)
  where
    toJSON e = Array $ VV.fromList (toJSON <$> listEnumeration e)

splitBy :: Char -> String -> (String, Maybe String)
splitBy c s =
  case findIndex (== c) s of
    Nothing -> (s, Nothing)
    Just i ->
      let (b, o) = splitAt (i + 1) s
       in (b, Just o)

normalizeUrl :: String -> NormalizedUrl
normalizeUrl s =
  let (url, mFragment) = splitBy '#' s
   in NormalizedUrl (UrlWithoutFragment url) mFragment

enumUrl :: String -> Enumeration UrlWithoutFragment -> (Enumeration UrlWithoutFragment, RefUrl)
enumUrl url e =
  let (NormalizedUrl uwf mf) = normalizeUrl url
      (e', ref) = enum uwf e
   in (e', RefUrl ref mf)

compressTarget :: Target -> DocState -> (DocState, CompressedTarget)
compressTarget target state =
  let (urlsEnum', urlRefUrl) = enumUrl (targetURL target) (_urlsEnum state)

      (urlsEnum'', packageEnum', mPackageRef) =
        case targetPackage target of
          Nothing -> (urlsEnum', _packageEnum state, Nothing)
          Just (p, url) ->
            let (urlsEnum'', refUrl) = enumUrl url urlsEnum'
                (packageEnum', packageRef) = enum (PackageName p) (_packageEnum state)
             in (urlsEnum'', packageEnum', Just (packageRef, refUrl))

      (urlsEnum''', moduleEnum', mModuleRef) =
        case (,) <$> targetPackage target <*> targetModule target of
          Nothing ->  (urlsEnum'', _moduleEnum state, Nothing)
          Just ((p, _), (m, url)) ->
            let packageName = (PackageName p)
                moduleName = ModuleName packageName m
                (urlsEnum''', urlRef) = enumUrl url urlsEnum''
                (moduleEnum', moduleRef) = enum moduleName (_moduleEnum state)
            in (urlsEnum''', moduleEnum', Just (moduleRef, urlRef))

      tt = targetType target
      ctt =
        if | tt == "package" -> PackageType
           | tt == "module" -> ModuleType
           | otherwise -> NoneType

      compressedTarget = CompressedTarget
        {
          nTargetURL = urlRefUrl,
          nTargetPackage = mPackageRef,
          nTargetModule = mModuleRef,
          nTargetType = ctt,
          nTargetItem = targetItem target,
          nTargetDocs = targetDocs target
        }
  in
    (state { _urlsEnum = urlsEnum''',
             _packageEnum = packageEnum',
             _moduleEnum = moduleEnum'
           } , compressedTarget)


mkMap :: [Target] -> Map TargetId Target
mkMap tgts =
  M.fromList $ zipWith (\i t -> (TargetId i, t)) [0..] tgts

addPDoc t pdocs =
  case (targetType t, targetURL t, targetPackage t) of
    ("module", url, Just (package, _)) ->

      M.insertWith
        (\pdNew pdOld -> pdOld {_modules = _modules pdOld ++ _modules pdNew} )
        (T.pack package)
        (PackageDocs
          Nothing
          [(T.pack $ (drop 7 (unHTML (targetItem t))),
            T.pack $ targetURL t)])
        pdocs

    ("package", url, Nothing) ->
      let package = T.pack (drop 8 (unHTML (targetItem t))) in
      M.insertWith
        (\pdNew pdOld -> pdOld {_contentsUrl = _contentsUrl pdNew} )
        package
        (PackageDocs (Just (T.pack url)) [])
        pdocs

    _ -> pdocs

aggItem
  :: (DocState, [(Text, CompressedTarget)], Map Text PackageDocs)
  -> (BStr, Target)
  -> (DocState, [(Text, CompressedTarget)], Map Text PackageDocs)
aggItem (state, acc, pdocs) (k, t) =
   let (state', c) = compressTarget t state
    in (state', ((decodeUtf8 k, c):acc), (addPDoc t pdocs))

groupByKeys :: Ord a => [(a, b)] -> Map a [b]
groupByKeys xs = foldl' f M.empty xs
  where
    f m (k, v) = M.insertWith (++) k [v] m

genDb :: FilePath -> IO ()
genDb fn = hoogle [ "generate", "--database=" <> fn, "-l" ]

writeAsset :: ToJSON a => FilePath -> a -> String -> IO ()
writeAsset dir_ x name = do
  let fn = dir_ </> (name <> ".js")
      content =
        ( "window.hdassets = window.hdassets ? window.hdassets : {};"
            <> "window.hdassets[\""
            <> TL.pack name
            <> "\"] = "
            <> encodeToLazyText x
            <> ";"
        )
  TL.writeFile fn content

ghcPkgs = do
  settings <- loadSettings
  keys <- M.keys <$> readGhcPkg settings
  pure $ show <$> keys

contentMatches :: FilePath -> String -> IO Bool
contentMatches fn s = do
  exists <- doesFileExist fn
  if exists
    then do
      cached <- Prelude.readFile fn
      pure (s == cached)
    else pure False

indexIsValid :: FilePath -> FilePath -> IO Bool
indexIsValid hoogleDbFile genDocsDir = do
  s <- show <$> getModificationTime hoogleDbFile
  (genDocsDir </> "hoogledb.txt") `contentMatches` s

markIndexIsValid :: FilePath -> FilePath -> IO ()
markIndexIsValid hoogleDbFile genDocsDir = do
  s <- show <$> getModificationTime hoogleDbFile
  Prelude.writeFile (genDocsDir </> "hoogledb.txt") s


copyAssets :: FilePath -> IO ()
copyAssets dstDir = do
  let fns = ["html/frontend.e31bb0bc.js", "html/style.c61ab535.css", "html/index.html"]
  forM_ fns $ \fn -> do
    src <- getDataFileName fn
    copyFile src (dstDir </> fn)

---------------------------------------------------------------------------------

mapMaybeM :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
mapMaybeM Nothing f = pure Nothing
mapMaybeM (Just x) f = f x

findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str)

extractDB :: FilePath -> IO (Maybe FilePath)
extractDB fn = do
  s <- readFile fn
  let (a, _, _, xs) = ((s :: String) =~ ("--database ([^ ]+)" :: String)) :: (String, String, String, [String])
  pure (listToMaybe xs)

searchNixHoogleDB :: IO (Maybe FilePath)
searchNixHoogleDB = do
  fn <- findExecutable "hoogle"
  mapMaybeM fn $ \f' -> do
    f <- canonicalizePath f'
    size <- getFileSize f
    if size < 5000
       then extractDB f
       else pure (Nothing)

findStackHoogleDB' :: FilePath -> IO (Maybe FilePath)
findStackHoogleDB' dir =
  listToMaybe <$> searchFiles "database.hoo" dir
    where searchFiles pat = Find.find Find.always (Find.fileName Find.~~? pat)

searchStackHoogleDB :: FilePath ->  IO (Maybe FilePath)
searchStackHoogleDB projectDir = do
  let hdir = projectDir </> "./.stack-work/hoogle"
  exists <- doesDirectoryExist hdir
  case exists of
    True -> findStackHoogleDB' hdir
    False -> pure Nothing

searchGeneratedHoogleDB :: FilePath -> IO (Maybe FilePath)
searchGeneratedHoogleDB projectDir = do
  let fn = projectDir </> "generated-docs/db.hoo"
  exists <- doesFileExist fn
  pure $ case exists of
    False -> Nothing
    True -> Just fn

searchGeneratedHoogleDBAndGen :: FilePath -> IO (Maybe FilePath)
searchGeneratedHoogleDBAndGen projectDir = do
  fn <- searchGeneratedHoogleDB projectDir
  case fn of
    Just f -> pure (Just f)
    Nothing -> do
      let fn = projectDir </> "generated-docs/db.hoo"
      genDb fn
      pure (Just fn)

type SearchStrategy = IO (Maybe FilePath)

search :: [IO (Maybe a)] -> IO (Maybe a)
search [] = pure Nothing
search (g:gs) = do
  mf <- g
  case mf of
    Just f -> pure (Just f)
    Nothing -> search gs

searchHoogleDB :: FilePath -> IO (Maybe (HoogleDBSource, FilePath))
searchHoogleDB projectDir =
  search
  [ fmap (GhcPkgGenerated, ) <$> searchGeneratedHoogleDB projectDir
  , fmap (StackHoogleDB, ) <$> searchStackHoogleDB projectDir
  , fmap (NixHoogleDB, ) <$> searchNixHoogleDB
  , fmap (GhcPkgGenerated, ) <$> searchGeneratedHoogleDBAndGen projectDir
  ]


data HoogleDBSource
  = GhcPkgGenerated
  | StackHoogleDB
  | NixHoogleDB
  deriving (Show, Eq, Ord)


getProjectName :: FilePath -> IO String
getProjectName dir =
  takeFileName <$> canonicalizePath dir

main :: IO ()
main = do
  let projectDir = "."

  mf <- searchHoogleDB projectDir
  (hoogleDBSource, hoogleDbFile) <- case mf of
    Nothing -> do
      hPutStr stderr "Could not find or create a Hoogle database"
      exitFailure
    Just f -> pure f

  putStrLn ("Hoogle DB: " <> show (hoogleDBSource, hoogleDbFile))

  let genDocsDir = projectDir </> "generated-docs"
      htmlDir = genDocsDir </> "html"

  createDirectoryIfMissing True genDocsDir
  createDirectoryIfMissing True htmlDir

  indexValid <- indexIsValid hoogleDbFile genDocsDir

  projectName <- getProjectName projectDir

  when (not indexValid) $ do

    storeReadFile hoogleDbFile $ \store -> do
      putStrLn "Converting index to json..."

      let namesText = storeRead store NamesText
          namesItems = V.toList (storeRead store NamesItems)
          items = zip (bstr0Split namesText) (lookupItem store <$> namesItems)
          (docstate, keyTargetPairs, pdocs) = foldl' aggItem (newDocState projectName, [], M.empty) items
          targets = groupByKeys keyTargetPairs

      writeAsset htmlDir targets "targets"
      writeAsset htmlDir docstate "docstate"
      writeAsset htmlDir pdocs "modules"

      copyAssets genDocsDir
      markIndexIsValid hoogleDbFile genDocsDir

  indexURI <- ("file://" <>) <$> canonicalizePath (htmlDir </> "index.html")
  putStrLn indexURI
  void $ openBrowser indexURI
