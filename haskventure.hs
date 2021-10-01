import Adventure
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


runAdventure :: Adventure -> IO GameState
runAdventure adventure = do
    putStrLn "We're having an adventure!"
    print adventure
    return Quitting

runGameLogic :: GameState -> IO GameState
runGameLogic ChoosingAdventure = do
    adventure <- runAdventureSelector
    return $ HavingAdventure adventure
runGameLogic (HavingAdventure a) = runAdventure a
runGameLogic _ = return Quitting

gameLoop :: GameState -> IO ()
gameLoop state = do
    newState <- runGameLogic state
    if newState == Quitting then return () else gameLoop newState

main :: IO ()
main = gameLoop ChoosingAdventure

