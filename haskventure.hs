getAdventures :: [String]
getAdventures = ["Sample adventure", "Simple adventure"]

printList :: [String] -> IO [()]
printList items = sequence $ map putStrLn items

multipleChoice :: [String] -> IO String
multipleChoice choices = do
    printList [(show i) ++ ". " ++ x | (i,x) <- zip [1..] choices]
    putStr "\nPlease make a selection: "
    getLine

main = do
    putStrLn "Welcome to Haskventure!\n"
    putStrLn "These are the available adventures:"
    --adventures <- getAdventures
    --myAdventure <- multipleChoice adventures
    myAdventure <- multipleChoice $ getAdventures
    putStrLn $ "You chose " ++ myAdventure

