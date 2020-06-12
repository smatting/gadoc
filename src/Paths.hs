module Paths_gadoc where

import Data.Version.Extra

version :: Version
version = makeVersion [0,0]

getDataDir :: IO FilePath
getDataDir = return "."
