module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s) 	= show s

renderJValue (JNumber n) 	= show n
renderJValue (JBool True)	= "true"
renderJValue (JBool False)	= "false"
renderJValue (JNull)		= "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
	where pairs [] = ""
		pairs ps = intercalate ", " (map renderJValue vs)

renderJValue (JArray a) = "[" ++ values a ++ "]"
	where values [] = ""
		values vs = intercalate ", " (map rendrJValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)



