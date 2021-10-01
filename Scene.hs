module Scene (
    Scene,
    runScene
) where

import Adventure

type Scene = String

runScene :: Adventure -> Scene -> Maybe Scene
runScene _ _ = Nothing

