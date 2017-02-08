{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Pandoc.Mermaid.Run
  ( toPng
  , toSvg
  , toTex
  ) where

import           Control.Monad        ((>=>))
import           Control.Monad.Catch
import           Data.List
import           System.Directory
import           System.Exit          (ExitCode (..))
import           System.FilePath
import           System.Process       hiding (callCommand)

import qualified Data.ByteString.Lazy as BL

withDir :: FilePath -> IO a -> IO a
withDir path f = do
  dir <- getCurrentDirectory
  setCurrentDirectory path
  result <- f
  setCurrentDirectory dir
  return result

inDir :: FilePath -> (FilePath -> IO a) -> IO a
inDir path f = do
  let (dir, file) = splitFileName path
  withDir dir $ f file

run :: String -> [String] -> IO ()
run name args = do
  (_,_,_,p) <- createProcess (proc name args) { delegate_ctlc = True, std_out = NoStream }
  e <- waitForProcess p
  return $ case e of
                ExitSuccess   -> ()
                ExitFailure n -> error $ "Command " ++ name ++ " exited with error code " ++ show n

ifNotExists :: FilePath -> (FilePath -> IO ()) -> IO FilePath
ifNotExists path io = do
  isFile <- doesFileExist path
  if isFile
  then return path
  else const path <$> io path

toPng :: FilePath -> IO FilePath
toPng src = ifNotExists (src -<.> "png") $ \ png -> do
  let fromCommand = src <.> "png"
  inDir src $ \ file -> run "mermaid" ["-p", file]
  renameFile fromCommand png

toSvg :: FilePath -> IO FilePath
toSvg src = ifNotExists (src -<.> "svg") $ \ svg -> do
  let fromCommand = src <.> "svg"
  inDir src $ \ file -> run "mermaid" ["-s", file]
  fixSvg fromCommand svg
  removeFile fromCommand

toTex :: FilePath -> IO FilePath
toTex src = ifNotExists (src -<.> "tex") $ \ tex -> do
  let pdf     = src -<.> "pdf"
  let pdf_tex = src -<.> "pdf_tex"
  inDir src $ makeAbsolute >=> \ path -> run "inkscape" ["-D", "-z", "--file=" ++ path, "--export-pdf=" ++ (path -<.> "pdf"), "--export-latex"]
  renameFile pdf_tex tex

fixSvg :: FilePath -> FilePath -> IO ()
fixSvg old new = do
  content <- readFile old
  let newCont = fixSvgContent content
  writeFile new newCont

fixSvgContent :: String -> String
fixSvgContent = unlines . filter svgLineFilter . lines

svgLineFilter :: String -> Bool
svgLineFilter line =
    length line > 50
 || not (cssFilter line)

cssFilter :: String -> Bool
cssFilter line =
    "marker-end: \"url(#arrowhead)\";" `isInfixOf` line
 || "stroke-dasharray: \"2 2\";" `isInfixOf` line
