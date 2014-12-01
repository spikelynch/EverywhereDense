-- 

import Data.Char (ord, isLower, isUpper, toLower, isLetter)
import Data.List (intercalate)
import System.Random (randomRIO)

antlist :: Char -> [ String ]
antlist 'a' = [ "a", "an", "anyone", "anvil", "acronym", "ask", "arcane" ]
antlist 'b' = [ "by", "beg", "black", "but", "burden", "be", "bunyip" ]
antlist 'c' = [ "could", "cyan", "cloud", "can't", "canyon", "cold" ]
antlist 'd' = [ "don't", "do", "dig", "death", "did", "dense", "down" ]
antlist 'e' = [ "everywhere", "even", "extreme", "either", "egg" ]
antlist 'f' = [ "from", "for", "feather", "freezing", "felt" ]
antlist 'g' = [ "go",  "good", "green", "grass", "get", "golden", "god" ]
antlist 'h' = [ "have",  "heart", "help", "happy", "hard", "hey", "hot", "he", "his", "her" ]
antlist 'i' = [ "I", "it", "is", "isn't",  "iron", "incomplete", "into", "intimate", "I'll", "in" ]
antlist 'j' = [ "join", "joy",  "journey", "jail", "jump", "jazz" ]
antlist 'k' = [ "knit", "keepsake",  "know", "knife", "kept" ]
antlist 'l' = [ "let",  "length", "love", "learn", "lead", "lay" ]
antlist 'm' = [ "my", "me",  "marry", "mauve", "more", "melt", "maybe", "man"  ]
antlist 'n' = [ "no", "not",  "next", "need", "naked", "network" ]
antlist 'o' = [ "oh",   "ordinary", "other", "on", "out", "of", "off"  ]
antlist 'p' = [ "pink",  "pass", "put", "package", "purpose", "paid" ]
antlist 'q' = [ "quiet", "queen", "queue", "quiz", "quite", "quake", "quaint" ]
antlist 'r' = [  "random", "red", "related", "rode", "rot", "round"  ]
antlist 's' = [  "she", "sex", "sure", "sent", "sudden", "silver", "sigh", "sort"  ]
antlist 't' = [  "to", "the", "tale", "tall", "take", "toward", "tongue"  ]
antlist 'u' = [  "under", "up", "until", "upset", "unless"  ]
antlist 'v' = [ "version", "vow", "view", "vision", "vary", "verb" ]
antlist 'w' = [ "woman", "wild", "when", "were", "wizened", "we", "won't"  ]
antlist 'x' = [  "x-ray", "X-Men", "x-rated", "XXXX", "xenon"  ]
antlist 'y' = [ "yes", "yesterday", "year", "yet", "your", "you", "yearn" ]
antlist 'z' = [ "zither", "zebra", "zoo", "zip", "zoom" ]
antlist _   = []

antlist_i :: Char -> [ String ]
antlist_i c = case isUpper c of
               True -> antlist $ toLower c
               False -> antlist c


randrange :: [ a ] -> IO Int
randrange list = randomRIO (0, (length list) -1)


randant :: Char -> IO String
randant c = do
  words <- return (antlist_i c)
  case null words of
    True -> return ("" :: String )
    False -> do
              n <- randrange words
              return $ words !! n


initStr = "everywhere dense" :: String
iters = 3 :: Int


explode :: Int -> String -> IO [ String ]
explode n s = do
  words <- mapM randant s
  case n of
    0 -> return words
    otherwise -> explode (n - 1) (intercalate "" words)

formatwords :: [ String ] -> String
formatwords =  intercalate " " 


main :: IO ()
main = do 
  result <- explode iters initStr
  putStrLn $ intercalate " " result
  putStrLn $ show $ length result
