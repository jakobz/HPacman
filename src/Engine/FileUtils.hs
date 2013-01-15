module Engine.FileUtils where

import System.Directory
import Control.Monad
import Data.List (isSuffixOf)

hasExtension ext = isSuffixOf ("." ++ ext)

getFilesByExt ext dir = getCurrentDirectory
              >>= (\d -> return (d ++ "\\" ++ dir))
              >>= getDirectoryContents 
              >>= filterM (return . hasExtension ext)
              >>= mapM (return . (\d -> dir ++ "\\" ++ d))