import AdventureSelect

data GameState
    = ChoosingAdventure
    | HavingAdventure Adventure
    | Quitting


instance Eq GameState where
    ChoosingAdventure == ChoosingAdventure = True
    HavingAdventure x == HavingAdventure y = x == y
    Quitting == Quitting = True
    _ == _ = False

runGameLogic :: GameState -> IO GameState
runGameLogic ChoosingAdventure = do
    adventure <- runAdventureSelector
    return $ HavingAdventure adventure
runGameLogic _ = return Quitting

gameLoop :: GameState -> IO ()
gameLoop state = do
    newState <- runGameLogic state
    if newState == Quitting then return () else gameLoop newState

main :: IO ()
main = gameLoop ChoosingAdventure

