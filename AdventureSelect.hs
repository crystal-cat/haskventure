module AdventureSelect (
    runAdventureSelector,
    extractJust
) where


import Adventure
import MultipleChoice
import Utils

import System.Directory
import System.FilePath


getAdventures :: IO [Adventure]
getAdventures = do
    subdirs <- listDirectory "adventures"
    maybeAdventures <- sequence $ [tryReadAdventure ("adventures" </> dir </> "about.yaml") | dir <- subdirs]
    return $ extractJust maybeAdventures

runAdventureSelector :: IO Adventure
runAdventureSelector = do
    putStrLn "Welcome to Haskventure!\n"
    putStrLn "These are the available adventures:"
    adventures <- getAdventures
    multipleChoice "\nPlease make a selection: " adventures

