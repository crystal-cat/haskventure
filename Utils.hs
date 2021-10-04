module Utils (
    extractJust,
    onValueDo
) where

extractJust :: [Maybe a] -> [a]
extractJust a = [x | (Just x) <- a]

onValueDo :: (a -> b) -> b -> (Maybe a -> b)
onValueDo f defval = (\x ->
    case x of
        Just x' -> f x'
        Nothing -> defval)

