-- 

import Data.Char (ord)
import Data.List (intercalate)

nato = [ "ALFA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT", "GOLF", "HOTEL", "INDIA", "JULIETT", "KILO", "LIMA", "MIKE", "NOVEMBER", "OSCAR", "PAPA", "QUEBEC", "ROMEO", "SIERRA", "TANGO", "UNIFORM", "VICTOR", "WHISKEY", "XRAY", "YANKEE", "ZULU" ]

edense = [ "azimuth", "boromir", "cordite", "darkens", "element", "flanger", "germane", "harness", "inscape", "jumpers", "knights", "leopard", "manxman", "neonate", "origins", "popular", "quitted", "removes", "stretch", "tongues", "unscrew", "varmint", "warmest", "xoanons", "yttrium", "zetetic" ]

edense_f :: Char -> String
edense_f c
    | c >= 'a' && c <= 'z' = edense !! (ord c - 97)
    | c >= 'A' && c <= 'Z' = edense !! (ord c - 65)
    | otherwise            = []

antacronym :: (Char -> String) -> String -> String
antacronym fn s = intercalate " " words
    where words = filter (not . null) $ map fn s

explode :: Int -> String -> String
explode n s = explosions !! n
    where explosions = iterate (antacronym edense_f) s
