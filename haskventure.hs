{-# LANGUAGE DeriveGeneric #-}

import Data.Yaml
import GHC.Generics
import System.Directory
import System.FilePath

import MultipleChoice


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

main = do
    putStrLn "Welcome to Haskventure!\n"
    putStrLn "These are the available adventures:"
    adventures <- getAdventures
    myAdventure <- multipleChoice adventures
    putStrLn $ "You chose " ++ myAdventure

