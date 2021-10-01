{-# LANGUAGE DeriveGeneric #-}

module Adventure (
    Adventure,
    tryReadAdventure
) where

import Data.Yaml
import GHC.Generics
import System.FilePath


data Adventure = Adventure {
    name :: String,
    author :: String,
    version :: String
} deriving (Generic, Eq)

instance FromJSON Adventure

instance Show Adventure where
    show (Adventure name author version) =
        name ++ ", version " ++ version ++ ", by " ++ author

tryReadAdventure :: FilePath -> IO (Maybe Adventure)
tryReadAdventure = decodeFile

