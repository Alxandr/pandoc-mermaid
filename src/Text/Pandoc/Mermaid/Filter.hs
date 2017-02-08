{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Pandoc.Mermaid.Filter (
    mermaidFilter
  ) where

import           Control.Monad
import           Crypto.Hash
import           Data.Aeson
import           Data.Data
import           Data.List                  (any)
import           Data.Maybe
import           Data.Version               (showVersion)
import           Paths_pandoc_mermaid       (version)
import           System.Directory
import           System.Environment         (getArgs)
import           System.FilePath
import           Text.Pandoc.Definition
import           Text.Pandoc.Generic
import           Text.Pandoc.Mermaid.Run
import           Text.Pandoc.Walk

import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CL

class ToJSONFilter a where
  toJSONFilter :: a -> BL.ByteString -> IO BL.ByteString

instance (Walkable a Pandoc) => ToJSONFilter (a -> a) where
  toJSONFilter f =
    return . encode . (walk f :: Pandoc -> Pandoc) . either error id . eitherDecode'

instance (Walkable a Pandoc) => ToJSONFilter (a -> IO a) where
  toJSONFilter f =
    fmap encode . (walkM f :: Pandoc -> IO Pandoc) . either error id . eitherDecode

instance Data a => ToJSONFilter (a -> [a]) where
  toJSONFilter f =
    return . encode . (bottomUp (concatMap f) :: Pandoc -> Pandoc) .
    either error id . eitherDecode'

instance Data a => ToJSONFilter (a -> IO [a]) where
  toJSONFilter f =
    fmap encode . (bottomUpM (fmap concat . mapM f) :: Pandoc -> IO Pandoc) .
    either error id . eitherDecode'

instance (ToJSONFilter a) => ToJSONFilter ([String] -> a) where
  toJSONFilter f = \str -> getArgs >>= \args -> toJSONFilter (f args) str

instance (ToJSONFilter a) => ToJSONFilter (Maybe Format -> a) where
  toJSONFilter f = \str -> getArgs >>= \args -> toJSONFilter (f $ fmap Format $ listToMaybe args) str

mermaidFilter :: BL.ByteString -> IO BL.ByteString
mermaidFilter = toJSONFilter doMermaid

doMermaid :: Maybe Format -> Block -> IO Block
doMermaid format cb@(CodeBlock attr@(id, classes, namevals) contents) =
  case lookup "mermaid" namevals of
       Just f  -> convert format contents attr
       Nothing -> if "mermaid" `elem` classes
                  then convert format contents attr
                  else return cb
doMermaid target x = return x

convert :: Maybe Format -> String -> Attr -> IO Block
convert (Just (Format "latex")) = convertLatex
convert (Just (Format "html5")) = convertHtml
convert (Just (Format "html"))  = convertHtml
convert _                       = error "Format not supported"

(>->) :: Monad m => (a -> m b) -> (b -> c -> m d) -> (a -> c -> m d)
infixr 1 >->
(l >-> r) a c = l a >>= \ b -> r b c

convertLatex :: String -> Attr -> IO Block
convertLatex = convertToSvg >-> createInsert

convertHtml :: String -> Attr -> IO Block
convertHtml = convertToSvg >-> createImage

convertToSvg :: String -> IO FilePath
convertToSvg = sourceFile >=> toSvg

sourceFile :: String -> IO FilePath
sourceFile content = do
  let content' = C.pack $ content ++ showVersion version
  let hash' = show $ hashWith MD5 content'
  let dir = ".mermaid"
  unlessM (doesDirectoryExist dir) (createDirectory dir)

  let file = dir </> hash' <.> "mmd"
  writeFile file content
  return file

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test block = test >>= \r -> unless r block

createInsert :: FilePath -> Attr -> IO Block
createInsert svg attr = do
  latex <- toTex svg
  return $ RawBlock (Format "latex") $ createFigure latex attr

createImage :: FilePath -> Attr -> IO Block
createImage img attr = return $ Para [Image attr [] (img, "figure")]

createFigure :: FilePath -> Attr -> String
createFigure path (key, classes, attrs) = wrapped
  where (dir, file) = splitFileName path
        ifAttr name result       = result <$> lookup name attrs
        include                  = Just $ "\\import{" ++ dir ++ "}{" ++ file ++ "}"
        label | key == ""        = Nothing
              | key /= ""        = Just $ "\\label{" ++ key ++ "}"
        caption                  = ifAttr "caption" $ \ caption -> "\\caption{" ++ caption ++ "}"
        width'                   = ifAttr "width" $ (\ width -> "\\def\\svgwidth{" ++ width ++ "}") . latexWidth
        width | isNothing width' = Just "\\def\\svgwidth{\\textwidth}"
              | isJust width'    = width'
        parts                    = catMaybes [width, include, label, caption]
        indented                 = fmap ("  " ++) parts
        wrapped                  = "\\begin{figure}\n" ++ unlines indented ++ "\n\\end{figure}\n"

latexWidth :: String -> String
latexWidth "full" = "\\textwidth"
latexWidth s      = s
