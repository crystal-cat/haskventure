module Utils (
    extractJust
) where

extractJust :: [Maybe a] -> [a]
extractJust a = [x | (Just x) <- a]

