{-# LANGUAGE DeriveGeneric #-}

import Data.Yaml
import GHC.Generics
import System.Directory
import System.FilePath

data Adventure = Adventure {
    name :: String,
    author :: String,
    version :: String
} deriving (Generic)

instance FromJSON Adventure

instance Show Adventure where
    show (Adventure name author version) =
        name ++ ", version " ++ version ++ ", by " ++ author

tryReadAdventure :: FilePath -> IO (Maybe Adventure)
tryReadAdventure = decodeFile

extractJust :: [Maybe a] -> [a]
extractJust a = [x | (Just x) <- a]

getAdventures :: IO [Adventure]
getAdventures = do
    subdirs <- listDirectory "adventures"
    maybeAdventures <- sequence $ [tryReadAdventure ("adventures" </> dir </> "about.yaml") | dir <- subdirs]
    return $ extractJust maybeAdventures

printList :: [String] -> IO [()]
printList items = sequence $ map putStrLn items

getChoice :: [String] -> IO String
getChoice valid_opts = do
    putStr "\nPlease make a selection: "
    choice <- getLine
    if choice `elem` valid_opts then return choice else getChoice valid_opts

multipleChoice :: Show a => [a] -> IO String
multipleChoice choices = do
    let enumChoices = zip (map show [1..]) (map show choices)
    printList [i ++ ". " ++ x | (i,x) <- enumChoices]
    getChoice . fst $ unzip enumChoices

main = do
    putStrLn "Welcome to Haskventure!\n"
    putStrLn "These are the available adventures:"
    adventures <- getAdventures
    myAdventure <- multipleChoice adventures
    putStrLn $ "You chose " ++ myAdventure

