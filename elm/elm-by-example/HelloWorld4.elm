module HelloWorld4 where

import Markdown

import Graphics.Element exposing (Element)

main : Element
main =
    Markdown.toElement """

    #Hello Hello

    Whooho markdown output of the HelloWorld4.elm program.

    ---
    """

