{-# LANGUAGE ScopedTypeVariables #-}
module Hmud.Util where

import qualified Random
import Hmud.Character

randomEnum :: forall a. (Enum a, Bounded a) => IO a
randomEnum = do
  let minBoundInt = fromEnum (minBound :: a)
  let maxBoundInt = fromEnum (maxBound :: a)
  randomInt <- Random.randomRIO (minBoundInt, maxBoundInt)
  return $ toEnum randomInt

randomCharacter :: String -> IO Character
randomCharacter name = do
  race <- randomEnum
  role <- randomEnum
  gender <- randomEnum
  level <- Random.randomRIO (1,99)
  return Character { charName = name
                   , charRace = race
                   , charRole = role
                   , charGender = gender
                   , charLevel = level
                   , charInventory = []
                   }
