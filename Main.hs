import Data.List
import Data.Traversable
import Data.Foldable
import System.IO
import System.Directory 
import Data.Time.Clock


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


countFileData :: String -> String -> IO()
countFileData root f = do
    print ( ">>> parsing: " ++ f )

    handle <- openFile (root ++ f) ReadMode
    contents <- hGetContents handle
    let cLines = lines contents
    putStrLn ( ">> lines: " ++ (show $ length cLines) )
    
    let allWords = words contents
    let wordsN = length allWords
    putStrLn (">> words: " ++ (show wordsN) )

    let vowelWords = filter (\w -> vowel w) allWords
    putStrLn ( ">> voweled: " ++ (show $ length vowelWords) )

    hClose handle 
    print "\n"
    --    (show f, length cLines, wordsN)


main :: IO()
main = do
    start <- getCurrentTime
    print start 

    let root = "/home/andrii/_data/"
    
    files <- listDirectory root

    for_ files (\f -> countFileData root f )

    end <- getCurrentTime
    let taken = diffUTCTime start end
    print taken
