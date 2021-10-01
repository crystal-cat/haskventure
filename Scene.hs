module Scene (
    Scene,
    runScene
) where

import Adventure
import System.FilePath
import System.IO

type Scene = String

getSceneFile :: Adventure -> Scene -> FilePath
getSceneFile a s = (getAdventurePath a) </> s ++ ".scene"

runScene :: Adventure -> Scene -> IO (Maybe Scene)
runScene adventure scene = do
    contents <- readFile $ getSceneFile adventure scene
    putStrLn contents
    return Nothing
