module Main where

import           System.Environment
import           System.IO
import           Text.Pandoc.Mermaid.Filter

import qualified Data.ByteString.Lazy       as LB

main :: IO ()
main = do
  args <- getArgs
  case args of
       ("load":name:args) -> fakeStdin name args
       _                  -> LB.getContents >>= mermaidFilter >>= LB.putStr

fakeStdin :: String -> [String] -> IO ()
fakeStdin name args = do
  str <- LB.readFile name
  result <- withArgs args $ mermaidFilter str
  LB.putStr result
