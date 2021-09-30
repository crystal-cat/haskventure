module MultipleChoice (
    multipleChoice
) where

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

