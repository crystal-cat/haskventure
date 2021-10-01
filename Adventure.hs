{-# LANGUAGE DeriveGeneric #-}

module Adventure (
    Adventure,
    AdventureData,
    getAdventurePath,
    getAdventureData,
    tryReadAdventure
) where

import Data.Yaml
import GHC.Generics
import System.FilePath


data AdventureData = AdventureData {
    name :: String,
    author :: String,
    version :: String
} deriving (Generic, Eq)

instance FromJSON AdventureData

instance Show AdventureData where
    show (AdventureData name author version) =
        name ++ ", version " ++ version ++ ", by " ++ author

data Adventure = Adventure FilePath AdventureData

instance Show Adventure where
    show (Adventure _ adata) = show adata

instance Eq Adventure where
    (Adventure _ d1) == (Adventure _ d2) = d1 == d2

getAdventurePath :: Adventure -> FilePath
getAdventurePath (Adventure path _) = path

getAdventureData :: Adventure -> AdventureData
getAdventureData (Adventure _ data_) = data_

tryReadAdventure :: FilePath -> IO (Maybe Adventure)
tryReadAdventure path = do
    maybeData <- decodeFileEither path
    case maybeData of
        Right advdata -> return . Just $ Adventure (takeDirectory path) advdata
        Left _ -> return Nothing
