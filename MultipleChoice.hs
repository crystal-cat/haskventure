module MultipleChoice (
    getChoice,
    multipleChoice
) where

import qualified Data.Map.Strict as Map

printList :: Show a => [a] -> IO [()]
printList items = sequence $ map (putStrLn . show) items

getChoice :: String -> [String] -> IO String
getChoice prompt valid_opts = do
    putStr prompt
    choice <- getLine
    if choice `elem` valid_opts then
        return choice
    else
        getChoice prompt valid_opts

multipleChoice :: Show a => String -> [a] -> IO a
multipleChoice prompt choices = do
    let enumChoices = zip (map show [1..]) choices
    let choicesMap = Map.fromList enumChoices
    printList [i ++ ". " ++ (show x) | (i,x) <- enumChoices]
    choiceIndex <- (getChoice prompt) . fst $ unzip enumChoices
    return $ choicesMap Map.! choiceIndex

