module Renderer (render) where

import Parser

render :: Maybe HtmlElement
render = Parser.parseHtml "<section dd=\"mmmm\"><a href=\"dawdawda\">daaaaaa</a><img src=\"dawdawda\" alt=\"alt data\" /></section>" -- TEMP
