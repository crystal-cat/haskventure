module MultipleChoice (
    multipleChoice
) where

import qualified Data.Map.Strict as Map

printList :: Show a => [a] -> IO [()]
printList items = sequence $ map (putStrLn . show) items

getChoice :: [String] -> IO String
getChoice valid_opts = do
    putStr "\nPlease make a selection: "
    choice <- getLine
    if choice `elem` valid_opts then return choice else getChoice valid_opts

multipleChoice :: Show a => [a] -> IO a
multipleChoice choices = do
    let enumChoices = zip (map show [1..]) choices
    let choicesMap = Map.fromList enumChoices
    printList [i ++ ". " ++ (show x) | (i,x) <- enumChoices]
    choiceIndex <- getChoice . fst $ unzip enumChoices
    return $ choicesMap Map.! choiceIndex
