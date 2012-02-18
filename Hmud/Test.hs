module Hmud.Test where

import Test.Hspec.HUnit
import Test.Hspec
import Test.HUnit
import Data.Maybe (fromJust, isNothing)
import Data.Either.Unwrap
import qualified Data.Map as Map
import Data.Map (Map)

import Hmud.World
import Hmud.Character
import Hmud.Room
import Hmud.TestData
import Xmpp.Users

player0 = Character
  { charName = "player0"
  , charRace = Human
  , charRole = Fool
  , charGender = Female
  , charLevel = 1
  , charInventory = []
  , charAddress = Address ""
  }

emptyWorld = World { worldRooms = [] }

specs :: Specs
specs = descriptions
  [ describe "insertCharacterToRoom"
    [ it "returns Nothing when there is no such room in the world"
        (either (const True) (const False) $ insertCharacterToRoom player0 "what where?" emptyWorld)
    , it "returns Nothing when there is no such room in the world"
        (either (const True) (const False) $ insertCharacterToRoom player0 "what where?" world)
    , it "returns the world where the player is in the desired room on success"
        ((do
          w <- insertCharacterToRoom player0 "The Black Unicorn" world
          r <- findRoom "The Black Unicorn" w
          findCharacter "player0" r
        ) == Right player0)
    , it "works with abbreviated room name"
        ((do
          w <- insertCharacterToRoom player0 "The Bl" world
          r <- findRoom "The Black Unicorn" w
          findCharacter "player0" r
        ) == Right player0)
    ]

  , describe "gotoFromTo"
    [ it "returns Nothing if there is no such *from* room"
        (either (const True) (const False) $ gotoFromTo "player0" "what where?" "The Black Unicorn" world)
    , it "returns Nothing if there is no such *to* room"
        (either (const True) (const False) $ gotoFromTo "player0" "The Black Unicorn" "what where?" world)
    , it "returns Nothing if there is no such character in the *from* room"
        (either (const True) (const False) $ gotoFromTo "slayer0" "The Black Unicorn" "town square" world)
    , it "works when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  let (w3, _, _, _) = fromRight $ gotoFromTo "player0" "The Black Unicorn" "town square" w2
                  let fromRoom = fromRight $ findRoom "The Black Unicorn" w3
                  let toRoom   = fromRight $ findRoom "town square" w3
                  assertBool "player is no longer in *fromRoom*" $ isLeft (findCharacter "player0" fromRoom)
                  assertEqual "player is now in *toRoom*" (Right player0) (findCharacter "player0" toRoom)
      )
    , it "works with abbreviated names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black" world
                  let (w3, _, _, _) = fromRight $ gotoFromTo "pl" "Th" "to" w2
                  let fromRoom = fromRight $ findRoom "The Blac" w3
                  let toRoom   = fromRight $ findRoom "tow" w3
                  assertBool "player is no longer in *fromRoom*" $ isLeft (findCharacter "player" fromRoom)
                  assertEqual "player is now in *toRoom*" (Right player0) (findCharacter "play" toRoom)
      )
    , it "fails when trying to go to the same room again"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertBool "going to the same room fails" $ isLeft $ gotoFromTo "player0" "The Black" "The Black Un" w2
      )
    ]
  , describe "findCharacterExactly"
    [ it "succeeds when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertEqual "player found" (Right player0) (findCharacterExactly "player0" w2)
                  assertBool "player not found" $ isLeft (findCharacterExactly "slayer0" w2)
      )
    , it "fails in the empty world"
      (TestCase $ do
                  assertBool "player not found" $ isLeft (findCharacterExactly "player0" emptyWorld)
                  assertBool "player not found" $ isLeft (findCharacterExactly "slayer0" emptyWorld)
      )
    , it "fails with partial names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertBool "player not found" $ isLeft (findCharacterExactly "pla" w2)
      )
    ]

  -- Xmpp.Users
  , describe "jid2player"
    [ it "returns the capitalized resource for conference JIDs"
      (jid2player "gtf@conference.localhost/markus.klinik" == "Markus Klinik")
    , it "returns the capitalized username for personal JIDs"
      (jid2player "markus.klinik@localhost/Gajim" == "Markus Klinik")
    ]
  , describe "updateNick"
    [ it "works when a second mapping is added"
      ( TestCase $ do
          let (nicks, _) = updateNick "foobar" "markus.klinik@localhost/Gajim" (Map.empty, Map.empty)
          assertBool "foobar is mapped to mkl" $ (fromJust $ Map.lookup "foobar" nicks) == "markus.klinik@localhost/Gajim"
          let (nicks2, _) = updateNick "blah" "hans.wurst@localhost/Gajim" (nicks, Map.empty)
          assertBool "foobar is still mapped to mkl" $ (fromJust $ Map.lookup "foobar" nicks2) == "markus.klinik@localhost/Gajim"
          assertBool "blah is mapped to hans" $ (fromJust $ Map.lookup "blah" nicks2) == "hans.wurst@localhost/Gajim"
          let (nicks3, _) = updateNick "mkl" "markus.klinik@localhost/Gajim" (nicks2, Map.empty)
          assertBool "markus has now nick mkl" $ (fromJust $ Map.lookup "mkl" nicks3) == "markus.klinik@localhost/Gajim"
          assertBool "nick foobar no longer exists" $ (isNothing $ Map.lookup "foobar" nicks3)
      )
    ]

  ]

main = hspec specs
