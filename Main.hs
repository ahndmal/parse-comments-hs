import Data.List
import Data.Traversable
import Data.Foldable
import System.IO

data Comment = Comment { body :: String
                       , created :: String
                       , pageId :: String
                       , host :: String } deriving (Show)

subStr str from to = drop from (take to str)

isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

vowel :: String -> Bool
vowel s = any (\c -> isVowel c) s

parseComment :: String -> Comment
parseComment str = do
    let comment = Comment {
        body = subStr str 1 10,
        created = "",
        pageId = "",
        host = "" }
    comment

main :: IO()
main = do
    let root = "/home/andrii/_data/"
    handle <- openFile (root ++ "comments.log.2023-01-21.0") ReadMode
    contents <- hGetContents handle
    let cLines = lines contents
    
    putStrLn ( ">> lines: " ++ (show $ length cLines) )
    
    let allWords = words contents
    let wordsN = length allWords
    putStrLn (">> words: " ++ (show wordsN) )

    let vowelWords = filter (\w -> vowel w) allWords

    putStrLn ( ">> voweled: " ++ (show $ length vowelWords) )

    putStrLn $ allWords !! 0

    hClose handle  
    
