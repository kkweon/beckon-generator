{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : FileIO
Description : Generate files based on the filepath
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

Generates a file
-}
module FileIO
  ( generateBeckonFiles
  , GenerateFileTypeFlag(..)
  )
where

import           Text.Printf                    ( printf )
import           Control.Monad                  ( unless
                                                , when
                                                , replicateM_
                                                , forM_
                                                )
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar        ( MVar
                                                , takeMVar
                                                , putMVar
                                                , newEmptyMVar
                                                )
import qualified Data.Text.IO                  as IO
import qualified System.Directory              as D
import qualified System.FilePath.Posix         as F
import           Template                       ( BeckonFile(..)
                                                , BeckonGeneratedFile(..)
                                                )


data GenerateFileTypeFlag = All | SpecOnly | SrcOnly deriving (Eq)

class FileGeneratable a where
    grabFiles :: a -> GenerateFileTypeFlag ->  [BeckonFile]

instance FileGeneratable BeckonGeneratedFile where
    grabFiles BeckonGeneratedService { srcFile, specFile } flag =
        case flag of
            All -> [srcFile, specFile]
            SpecOnly ->  [specFile]
            SrcOnly ->  [srcFile]

    grabFiles BeckonGeneratedComponent { srcFile, tmplFile, specFile } flag =
        case flag of
            All -> [srcFile, tmplFile, specFile]
            SpecOnly ->  [specFile]
            SrcOnly ->  [srcFile, tmplFile]



-- | Generate Beckon Files
generateBeckonFiles
  :: GenerateFileTypeFlag
  -> Bool -- ^ FORCE
  -> BeckonGeneratedFile
  -> IO ()
generateBeckonFiles flag force beckonTarget = mgenerateBeckonFiles force files
  where files = grabFiles beckonTarget flag


-- | Generate files in parallel
mgenerateBeckonFiles
  :: Bool -- ^ force write
  -> [BeckonFile] -- ^ Beckon files
  -> IO ()
mgenerateBeckonFiles force xs = do
  messageMVar <- newEmptyMVar :: IO (MVar (Maybe String))
  forM_ xs
    $ \beckonFile -> forkIO $ generateBeckonFile beckonFile force messageMVar
  replicateM_ (length xs) $ do
    maybeMessage <- takeMVar messageMVar
    forM_ maybeMessage putStrLn



-- | Generate target
-- If fail, write message to messageChan
-- If directory does not exists, it creates
generateBeckonFile
  :: BeckonFile
  -> Bool -- ^ Force
  -> MVar (Maybe String)
  -> IO ()
generateBeckonFile BeckonFile { target, content } force messageChan = do
  let dirName = F.takeDirectory target
  doesDirExist <- D.doesDirectoryExist dirName
  unless doesDirExist (D.createDirectoryIfMissing True dirName)

  fileExists <- D.doesFileExist target
  writeFileHandler fileExists force
 where
  writeFileHandler True True =
    sendMessage ("Overwriting " ++ target) >> IO.writeFile target content
  writeFileHandler True  False = showErrorMsg
  writeFileHandler False _     = IO.writeFile target content >> succeed

  showErrorMsg :: IO ()
  showErrorMsg =
    sendMessage $ "Skipping a file (" ++ target ++ ") because it already exists"


  succeed :: IO ()
  succeed = putMVar messageChan Nothing

  sendMessage :: String -> IO ()
  sendMessage x = putMVar messageChan $ Just x
