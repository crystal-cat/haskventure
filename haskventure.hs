import Main.Utf8 (withUtf8)
import Text.Show.Unicode
import Adventure
import AdventureSelect
import Scene

data GameState
    = ChoosingAdventure
    | StartingAdventure Adventure
    | PlayingScene Scene
    | Quitting


instance Eq GameState where
    ChoosingAdventure == ChoosingAdventure = True
    StartingAdventure x == StartingAdventure y = x == y
    PlayingScene x == PlayingScene y = x == y
    Quitting == Quitting = True
    _ == _ = False


runAdventure :: Adventure -> IO GameState
runAdventure adventure = do
    let title = ushow adventure
    putStrLn ""
    putStrLn title
    putStrLn . (take $ length title) $ repeat '='
    putStrLn ""
    return $ PlayingScene $ getFirstScene adventure

runGameLogic :: GameState -> IO GameState
runGameLogic ChoosingAdventure = do
    adventure <- runAdventureSelector
    return $ StartingAdventure adventure
runGameLogic (StartingAdventure a) = runAdventure a
runGameLogic (PlayingScene s) = do
    nextScene <- runScene s
    case nextScene of
        Just newScene -> return $ PlayingScene newScene
        Nothing -> return Quitting
runGameLogic _ = return Quitting

gameLoop :: GameState -> IO ()
gameLoop state = do
    newState <- runGameLogic state
    if newState == Quitting then return () else gameLoop newState

main :: IO ()
main = withUtf8 $
    gameLoop ChoosingAdventure

