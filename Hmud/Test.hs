module Hmud.Test where

import Test.Hspec.HUnit
import Test.Hspec
import Test.HUnit
import Data.Maybe (fromJust, isNothing)
import Data.Either.Unwrap
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Control.Monad.State as State
import Control.Monad.State (State)

import Hmud.World
import Hmud.Character
import Hmud.Room
import Hmud.TestData
import Hmud.Hmud
import Hmud.Message
import Xmpp.Users

player0 = Character
  { charName = "player0"
  , charRace = Human
  , charRole = Fool
  , charGender = Female
  , charLevel = 1
  , charInventory = []
  , charAddress = Address "player0addr"
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
          findCharacterInRoom "player0" r
        ) == Right player0)
    , it "works with abbreviated room name"
        ((do
          w <- insertCharacterToRoom player0 "The Bl" world
          r <- findRoom "The Black Unicorn" w
          findCharacterInRoom "player0" r
        ) == Right player0)
    ]

  , describe "gotoFromTo"
    [ it "returns Nothing if there is no such *from* room"
        (either (const True) (const False) $ gotoFromTo (Address "player0addr") "what where?" "The Black Unicorn" world)
    , it "returns Nothing if there is no such *to* room"
        (either (const True) (const False) $ gotoFromTo (Address "player0addr") "The Black Unicorn" "what where?" world)
    , it "returns Nothing if there is no such character in the *from* room"
        (either (const True) (const False) $ gotoFromTo (Address "slayer0") "The Black Unicorn" "town square" world)
    , it "works when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  let (w3, _, _, _) = fromRight $ gotoFromTo (Address "player0addr") "The Black Unicorn" "town square" w2
                  let fromRoom = fromRight $ findRoom "The Black Unicorn" w3
                  let toRoom   = fromRight $ findRoom "town square" w3
                  assertBool "player is no longer in *fromRoom*" $ isLeft (findCharacterInRoom "player0" fromRoom)
                  assertEqual "player is now in *toRoom*" (Right player0) (findCharacterInRoom "player0" toRoom)
      )
    , it "does not work with abbreviated names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black" world
                  assertBool "no such player pl" $ isLeft $ gotoFromTo (Address "pl") "Th" "to" w2
      )
    , it "fails when trying to go to the same room again"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertBool "going to the same room fails" $ isLeft $ gotoFromTo (Address "player0addr") "The Black" "The Black Un" w2
      )
    ]
  , describe "findCharacterExactly"
    [ it "succeeds when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertEqual "player found" (Right player0) (findCharacterExactly (Address "player0addr") w2)
                  assertBool "player not found" $ isLeft (findCharacterExactly (Address "slayer0") w2)
      )
    , it "fails in the empty world"
      (TestCase $ do
                  assertBool "player not found" $ isLeft (findCharacterExactly (Address "player0addr") emptyWorld)
                  assertBool "player not found" $ isLeft (findCharacterExactly (Address "slayer0") emptyWorld)
      )
    , it "fails with partial names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertBool "player not found" $ isLeft (findCharacterExactly (Address "pla") w2)
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

  -- system tests, maps input messages to output messages
  , describe "system tests"
    [ it "a player joins, and is put to The Black Unicorn"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs)) = State.runState (run world) ([(MsgPlayerEnters (Address "player0") "Hel Mut")], []::[Message])
          assertBool "input messages are all consumed" $ null inputMsgs
          assertEqual "player0 is in the Unicorn"
            "The Black Unicorn" (roomName (fromRight $ findRoomOfPlayerExactly (Address "player0") newWorld))
      )
    , it "a player joins, then goes to town square"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs)) = State.runState (run world) (
                [ (MsgPlayerEnters (Address "player0") "Hel Mut")
                , (MsgCommand      (Address "player0") ["goto", "town square"])
                ], []::[Message])
          assertBool "input messages are all consumed" $ null inputMsgs
          assertEqual "player0 is in town square"
            "town square" (roomName (fromRight $ findRoomOfPlayerExactly (Address "player0") newWorld))
      )
    ]

  ]

main = hspec specs
