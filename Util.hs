module Util where

import qualified Random
import Character

randomRace :: IO Race
randomRace = do
  let minInt = fromEnum $ (minBound::Race)
  let maxInt = fromEnum $ (maxBound::Race)
  randInt <- Random.randomRIO (minInt, maxInt)
  return $ toEnum randInt

randomRole :: IO Role
randomRole = do
  let minInt = fromEnum $ (minBound::Role)
  let maxInt = fromEnum $ (maxBound::Role)
  randInt <- Random.randomRIO (minInt, maxInt)
  return $ toEnum randInt

randomGender :: IO Gender
randomGender = do
  let minInt = fromEnum $ (minBound::Gender)
  let maxInt = fromEnum $ (maxBound::Gender)
  randInt <- Random.randomRIO (minInt, maxInt)
  return $ toEnum randInt

randomCharacter :: String -> IO Character
randomCharacter name = do
  race <- randomRace
  role <- randomRole
  gender <- randomGender
  level <- Random.randomRIO (1,99)
  return Character { charName = name
                   , charRace = race
                   , charRole = role
                   , charGender = gender
                   , charLevel = level
                   }
