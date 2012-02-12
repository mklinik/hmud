module Hmud.Test where

import Test.Hspec.HUnit
import Test.Hspec
import Test.HUnit
import Data.Maybe (fromJust)

import Hmud.World
import Hmud.Character
import Hmud.Room
import Hmud.TestData

player0 = Character
  { charName = "player0"
  , charRace = Human
  , charRole = Fool
  , charGender = Female
  , charLevel = 1
  }

emptyWorld = World { worldRooms = [] }

specs :: Specs
specs = descriptions
  [ describe "insertCharacterToRoom"
    [ it "returns Nothing when there is no such room in the world"
        (insertCharacterToRoom player0 "what where?" emptyWorld == Nothing)
    , it "returns Nothing when there is no such room in the world"
        (insertCharacterToRoom player0 "what where?" world == Nothing)
    , it "returns the world where the player is in the desired room on success"
        ((do
          w <- insertCharacterToRoom player0 "The Black Unicorn" world
          r <- findRoom "The Black Unicorn" w
          findCharacter "player0" r
        ) == Just player0)
    , it "works with abbreviated room name"
        ((do
          w <- insertCharacterToRoom player0 "The Bl" world
          r <- findRoom "The Black Unicorn" w
          findCharacter "player0" r
        ) == Just player0)
    ]

  , describe "gotoFromTo"
    [ it "returns Nothing if there is no such *from* room"
        (gotoFromTo "player0" "what where?" "The Black Unicorn" world == Nothing)
    , it "returns Nothing if there is no such *to* room"
        (gotoFromTo "player0" "The Black Unicorn" "what where?" world == Nothing)
    , it "returns Nothing if there is no such character in the *from* room"
        (gotoFromTo "slayer0" "The Black Unicorn" "Town Square" world == Nothing)
    , it "works when everything is fine"
      (TestCase $ do
                  let w2 = fromJust $ insertCharacterToRoom player0 "The Black Unicorn" world
                  let w3 = fromJust $ gotoFromTo "player0" "The Black Unicorn" "Town Square" w2
                  let fromRoom = fromJust $ findRoom "The Black Unicorn" w3
                  let toRoom   = fromJust $ findRoom "Town Square" w3
                  assertEqual "player is no longer in *fromRoom*" Nothing (findCharacter "player0" fromRoom)
                  assertEqual "player is now in *toRoom*" (Just player0) (findCharacter "player0" toRoom)
      )
    ]
  ]

main = hspec specs
