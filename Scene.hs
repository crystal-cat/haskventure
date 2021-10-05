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
    | ChangeScene String
    | Quit

instance Show Action where
    show (Print str) = "Print " ++ str
    show (ChangeScene str) = "Go to scene " ++ str
    show Quit = "Quit"

data Command
    = Keyword String Action
    | RegexMatch Regex Action

instance Show Command where
    show (Keyword kw action) = "Keyword (" ++ kw ++ " -> " ++ (show action) ++ ")"


getSceneFile :: Adventure -> Scene -> FilePath
getSceneFile a s = (getAdventurePath a) </> s ++ ".scene"

isCommand :: String -> Bool
isCommand [] = False
isCommand (x:_) = (x == '!')

parseCommandMatch :: [String] -> Maybe Command
parseCommandMatch ["kw", kw, "print", msg] = Just $ Keyword kw $ Print msg
parseCommandMatch ["kw", kw, "scene", s] = Just $ Keyword kw $ ChangeScene s
parseCommandMatch ["regex", r, "print", msg] = Just $ RegexMatch (mkRegex r) $ Print msg
parseCommandMatch ["regex", r, "scene", s] = Just $ RegexMatch (mkRegex r) $ ChangeScene s
parseCommandMatch _ = Nothing

parseCommand :: String -> Maybe Command
parseCommand str = (m >>= parseCommandMatch) where
    r = mkRegex "!(kw|regex):(.*) -> (print|scene) (.*)"
    m = matchRegex r str

isCmdMatch :: String -> Command -> Bool
isCmdMatch str (Keyword kw _) = (kw == str)
isCmdMatch str (RegexMatch r _) = isRegexMatch r str

findActionForInput :: [Command] -> String -> Maybe Action
findActionForInput cmds str =
    case (find (isCmdMatch str) cmds) of
        Just (Keyword _ action) -> Just action
        Just (RegexMatch _ action) -> Just action
        _ -> Nothing

executeAction :: Scene -> Action -> IO (Maybe Scene)
executeAction _ Quit = return Nothing
executeAction thisScene (Print str) = do
    putStrLn str
    return $ Just thisScene
executeAction _ (ChangeScene newScene) = return $ Just newScene

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
    --uprint cmds
    runSceneLoop adventure cmds scene

runSceneLoop :: Adventure -> [Command] -> Scene -> IO (Maybe Scene)
runSceneLoop adventure cmds scene = do
    putStr "> "
    input <- getLine
    let maybeAction = findActionForInput cmds input
    result <- case maybeAction of
        Just action -> executeAction scene action
        Nothing -> return $ Just scene
    case result of
        Just newScene ->
            if newScene == scene then
                runSceneLoop adventure cmds scene
            else
                runScene adventure newScene
        Nothing ->
            return Nothing

