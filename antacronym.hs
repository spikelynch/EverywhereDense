-- 

import Data.Char (ord, isLower, isUpper, toLower, isLetter)
import Data.List (intercalate)
import System.Random (randomRIO)

antlist :: Char -> [ String ]
antlist 'a' = [ "a", "an", "anyone", "anvil", "acronym", "ask", "arcane", "Anne" ]
antlist 'b' = [ "by", "beg", "black", "Bob", "burden", "be", "bunyip" ]
antlist 'c' = [ "could", "cyan", "cloud", "Cindy", "can't", "canyon" ]
antlist 'd' = [ "don't", "do", "Dave", "dig", "death", "did", "dense" ]
antlist 'e' = [ "everywhere", "even", "Elsa", "extreme", "either", "egg" ]
antlist 'f' = [ "from", "Frank", "for", "feather", "freezing", "felt" ]
antlist 'g' = [ "go", "Gillian", "good", "green", "grass", "get", "golden" ]
antlist 'h' = [ "have", "Howard", "heart", "help", "happy", "hard", "hey" ]
antlist 'i' = [ "I", "it", "is", "isn't", "Ivy", "iron", "incomplete", "into", "intimate", "I'll", "in" ]
antlist 'j' = [ "join", "John", "journey", "jail", "jump", "jazz" ]
antlist 'k' = [ "knit", "keepsake", "Kathy", "know", "knife", "kept" ]
antlist 'l' = [ "let", "Leo", "length", "love", "learn", "lead", "lay" ]
antlist 'm' = [ "my", "me", "Mary", "marry", "mauve", "more", "melt", "maybe", "man"  ]
antlist 'n' = [ "no", "not", "Ned", "next", "need", "naked", "network" ]
antlist 'o' = [ "oh", "Olive", "olive", "ordinary", "other", "on", "out", "of", "off"  ]
antlist 'p' = [ "pink", "Peter", "pass", "put", "package", "purpose", "paid" ]
antlist 'q' = [ "quiet", "queen", "queue", "quiz", "quite", "quake", "quaint" ]
antlist 'r' = [ "Ron", "random", "red", "related", "rode", "rot", "round"  ]
antlist 's' = [ "Susan", "sex", "sure", "sent", "sudden", "silver", "sigh", "sort"  ]
antlist 't' = [ "Tom", "to", "the", "tale", "tall", "take", "toward", "tongue"  ]
antlist 'u' = [ "Ursula", "under", "up", "until", "upset", "unless"  ]
antlist 'v' = [ "Vern", "version", "vow", "view", "vision", "vary", "verb" ]
antlist 'w' = [ "woman", "wild", "Wilhelmina", "wizened", "we", "won't"  ]
antlist 'x' = [ "Xavier", "x-ray", "X-Men"  ]
antlist 'y' = [ "yes", "yesterday", "Yan", "yet", "your", "you", "yearn" ]
antlist 'z' = [ "Zack", "zither", "zebra", "zoo", "zip", "zoom" ]
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

antacronym :: String -> IO String
antacronym s = do
  words <- mapM randant s
  return $ formatwords (filter (not . null ) words)


explode :: Int -> String -> IO String
explode n s = do
  case n of
    0 -> return s
    otherwise -> do
                exploded <- antacronym s
                explode (n - 1) exploded

formatwords :: [ String ] -> String
formatwords =  intercalate " " 


main :: IO ()
main = do 
  input <- getLine
  result <- explode 6 input
  putStrLn result
  putStrLn $ show $ length result



  -- r1 <- antacronym initStr
  -- r2 <- antacronym r1
  -- r3 <- antacronym r2
  -- r4 <- antacronym r3
  -- r5 <- antacronym r4
  -- r6 <- antacronym r5
  -- r7 <- antacronym r6
