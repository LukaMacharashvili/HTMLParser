module Main where

import Parser
import Renderer

main :: IO ()
main = case Renderer.render of
  Just (Parser.HtmlElement _ _ _ children) -> do
    putStrLn $ "children: " ++ show children
  Nothing -> putStrLn "Nothing"
