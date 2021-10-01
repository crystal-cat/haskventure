module AdventureSelect (
    runAdventureSelector
) where


import Adventure
import MultipleChoice
import System.Directory
import System.FilePath


extractJust :: [Maybe a] -> [a]
extractJust a = [x | (Just x) <- a]

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
    multipleChoice adventures

