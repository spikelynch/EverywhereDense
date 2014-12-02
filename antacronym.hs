-- 

import Data.Char (ord, isLower, isUpper, toLower, isLetter, toUpper)
import Data.List (intercalate)
import System.Random (randomRIO)
import EverywhereDense


antlist = geometry_a


antlist_i :: Char -> [ String ]
antlist_i c = case isUpper c of
               True -> antlist $ toLower c
               False -> antlist c

expchar :: Char -> IO String
expchar c = do
  words <- return (antlist_i c)
  case null words of
    True -> return ("" :: String )
    False -> do
              n <- randrange words
              return $ words !! n

randrange :: [ a ] -> IO Int
randrange list = randomRIO (0, (length list) -1)


expWord :: String -> IO [ String ]
expWord= mapM expchar 

expSentence :: [ String ] -> IO [ [ String ] ]
expSentence = mapM expWord

expParagraph :: [ [ String ] ] -> IO [ [ [ String ] ] ]
expParagraph = mapM expSentence

expChapter :: [ [ [ String ] ] ] -> IO [ [ [ [ String ] ] ] ]
expChapter = mapM expParagraph

expSection :: [ [ [ [ String ] ] ] ] -> IO [ [ [ [ [ String ] ] ] ] ]
expSection = mapM expChapter


laySentence :: [ String ] -> String
laySentence sentence = (intercalate " " words) ++ "."
    where words = capfirst $ filter ( not . null ) sentence
          
capitalise [] = []
capitalise (c:cs) = (toUpper c):cs 

capfirst [] = []
capfirst (w:ws) = (capitalise w):ws

layParagraph :: [ [ String ] ] -> String
layParagraph paragraph = (intercalate " " sentences) ++ "\n\n"
    where sentences = map laySentence paragraph

layChapter :: [ [ [ String ] ] ] -> String
layChapter chapter = (heading '-' header) ++ "\n\n" ++ (concat body) ++ "\n\n"
    where (header, ps) = chapterHeader chapter 
          body = map layParagraph ps

chapterHeader ((s:ss):ps) = (s, ss:ps)
chapterHeader _           = ([], [])


laySection :: [ [ [ [ String ] ] ] ] -> String
laySection section = (heading '=' header) ++ "\n\n" ++ (concat body)
    where (header, cs) = sectionHeader section
          body = map layChapter cs
       

sectionHeader (((s:ss):ps):cs) = (s, (ss:ps):cs)
sectionHeader _                = ([], [])

layNovel :: [ [ [ [ [ String ] ] ] ] ] -> String
layNovel novel = concatMap laySection novel


heading :: Char -> [ String ] -> String
heading c s = (map toUpper sentence) ++ "\n" ++ (replicate (length sentence) c) ++ "\n"
    where sentence = intercalate " " $ filter ( not . null ) s




outputFile = "everywhere.dense.md" :: FilePath

initStr = "Everywhere dense is a generated novel based on the principal of recursive acronym expansion." :: String



main :: IO ()
main = do 
  sentence <- expWord initStr
  paragraph <- expSentence sentence
  chapter <- expParagraph paragraph
  section <- expChapter chapter
  novel <- expSection section
  writeFile outputFile $ layNovel novel
