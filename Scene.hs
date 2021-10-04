module Scene (
    Scene,
    runScene
) where

import Adventure
import MultipleChoice
import Utils

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import System.FilePath
import System.IO
import Text.Regex

type Scene = String

data Action
    = Print String
    | Quit

instance Show Action where
    show (Print str) = "Print " ++ str

data Command
    = Keyword String Action

instance Show Command where
    show (Keyword kw action) = "Keyword (" ++ kw ++ " -> " ++ (show action) ++ ")"


getSceneFile :: Adventure -> Scene -> FilePath
getSceneFile a s = (getAdventurePath a) </> s ++ ".scene"

isCommand :: String -> Bool
isCommand [] = False
isCommand (x:_) = (x == '!')

parseCommandMatch :: [String] -> Maybe Command
parseCommandMatch ["kw", kw, "print", msg] = Just $ Keyword kw $ Print msg
parseCommandMatch _ = Nothing

parseCommand :: String -> Maybe Command
parseCommand str = (m >>= parseCommandMatch) where
    r = mkRegex "!(kw):([a-zA-Z0-9]+) -> (print) (.*)"
    m = matchRegex r str

findActionForKeyword :: [Command] -> String -> IO (Maybe Action)
findActionForKeyword cmds kw =
    case (find (\(Keyword kw' action) -> (kw == kw')) cmds) of
        Just (Keyword _ action) -> return (Just action)
        _ -> return Nothing

executeAction :: Scene -> Action -> IO (Maybe Scene)
executeAction _ Quit = return Nothing
executeAction thisScene (Print str) = do
    putStrLn str
    return $ Just thisScene

getBuiltinCmds :: [Command]
getBuiltinCmds = [(Keyword "quit" Quit)]

getKeywords :: [Command] -> [String]
getKeywords cmds = [kw | (Keyword kw _) <- cmds]

processScene :: String -> (String, [Command])
processScene str = (text, getBuiltinCmds ++ cmds) where
    (cmds', text') = partition isCommand $ lines str
    cmds = extractJust $ map parseCommand cmds'
    text = unlines text'

runScene :: Adventure -> Scene -> IO (Maybe Scene)
runScene adventure scene = do
    contents <- readFile $ getSceneFile adventure scene
    let (text, cmds) = processScene contents
    putStrLn text
    --print cmds
    input <- getChoice "> " $ getKeywords cmds
    maybeAction <- findActionForKeyword cmds input
    case maybeAction of
        Just action -> executeAction scene action
        Nothing -> return $ Just scene
