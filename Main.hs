{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Parser

main :: IO ()
main = print $ Parser.parseHtml "<section dd=\"mmmm\"><a href=\"dawdawda\">daaaaaa</a><img src=\"dawdawda\" alt=\"alt data\" /></section>"