getAdventures :: [String]
getAdventures = ["Sample adventure", "Simple adventure"]

printList :: [String] -> IO [()]
printList items = sequence $ map putStrLn items

getChoice :: [String] -> IO String
getChoice valid_opts = do
    putStr "\nPlease make a selection: "
    choice <- getLine
    if choice `elem` valid_opts then return choice else getChoice valid_opts

multipleChoice :: [String] -> IO String
multipleChoice choices = do
    let enumChoices = zip (map show [1..]) choices
    printList [i ++ ". " ++ x | (i,x) <- enumChoices]
    getChoice . fst $ unzip enumChoices

main = do
    putStrLn "Welcome to Haskventure!\n"
    putStrLn "These are the available adventures:"
    --adventures <- getAdventures
    --myAdventure <- multipleChoice adventures
    myAdventure <- multipleChoice $ getAdventures
    putStrLn $ "You chose " ++ myAdventure

