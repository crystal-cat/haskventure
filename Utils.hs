module Utils (
    extractJust,
    isRegexMatch
) where

import Text.Regex

extractJust :: [Maybe a] -> [a]
extractJust a = [x | (Just x) <- a]

isRegexMatch :: Regex -> String -> Bool
isRegexMatch r str =
    case matchRegex r str of
        Just m -> True
        Nothing -> False

