module Hmud.Test where

import Test.Hspec.HUnit
import Test.Hspec
import Test.HUnit
import Data.Maybe (fromJust, isNothing)
import Data.Either.Unwrap
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import System.IO

import qualified Control.Monad.State as State
import Control.Monad.State (State)

import Hmud.World
import Hmud.Character
import Hmud.Room
import Hmud.TestData
import Hmud.Hmud
import Hmud.Message
import Hmud.Item
import Hmud.Commands
import Xmpp.Util

player0 = Character
  { charName = "player0"
  , charRace = Human
  , charRole = Fool
  , charGender = Female
  , charLevel = 1
  , charInventory = []
  , charAddress = Just "player0addr"
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
        (either (const True) (const False) $ gotoFromTo (Just "player0addr") "what where?" "The Black Unicorn" world)
    , it "returns Nothing if there is no such *to* room"
        (either (const True) (const False) $ gotoFromTo (Just "player0addr") "The Black Unicorn" "what where?" world)
    , it "returns Nothing if there is no such character in the *from* room"
        (either (const True) (const False) $ gotoFromTo (Just "slayer0") "The Black Unicorn" "town square" world)
    , it "works when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  let (w3, _, _, _) = fromRight $ gotoFromTo (Just "player0addr") "The Black Unicorn" "town square" w2
                  let fromRoom = fromRight $ findRoom "The Black Unicorn" w3
                  let toRoom   = fromRight $ findRoom "town square" w3
                  assertBool "player is no longer in *fromRoom*" $ isLeft (findCharacterInRoom "player0" fromRoom)
                  assertEqual "player is now in *toRoom*" (Right player0) (findCharacterInRoom "player0" toRoom)
      )
    , it "does not work with abbreviated names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black" world
                  assertBool "no such player pl" $ isLeft $ gotoFromTo (Just "pl") "Th" "to" w2
      )
    , it "fails when trying to go to the same room again"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertBool "going to the same room fails" $ isLeft $ gotoFromTo (Just "player0addr") "The Black" "The Black Un" w2
      )
    ]
  , describe "findCharacterExactly"
    [ it "succeeds when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertEqual "player found" (Right player0) (findCharacterExactly (Just "player0addr") w2)
                  assertBool "player not found" $ isLeft (findCharacterExactly (Just "slayer0") w2)
      )
    , it "fails in the empty world"
      (TestCase $ do
                  assertBool "player not found" $ isLeft (findCharacterExactly (Just "player0addr") emptyWorld)
                  assertBool "player not found" $ isLeft (findCharacterExactly (Just "slayer0") emptyWorld)
      )
    , it "fails with partial names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "The Black Unicorn" world
                  assertBool "player not found" $ isLeft (findCharacterExactly (Just "pla") w2)
      )
    ]

  -- Xmpp.Users
  , describe "jid2player"
    [ it "returns the capitalized resource for conference JIDs"
      (jid2player "gtf@conference.localhost/markus.klinik" == "Markus Klinik")
    , it "returns the capitalized username for personal JIDs"
      (jid2player "markus.klinik@localhost/Gajim" == "Markus Klinik")
    ]

  -- system tests, maps input messages to output messages
  , describe "system tests"
    [ it "a player joins, and is put to The Black Unicorn"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs)) = State.runState (run world) ([(MsgPlayerEnters (Just "player0") "Hel Mut")], []::[Message])
          assertBool "input messages are all consumed" $ null inputMsgs
          assertEqual "player0 is in the Unicorn"
            "The Black Unicorn" (roomName (fromRight $ findRoomOfPlayerExactly (Just "player0") newWorld))
      )
    , it "a player joins, then goes to town square"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs)) = State.runState (run world) (
                [ (MsgPlayerEnters (Just "player0") "Hel Mut")
                , (MsgCommand      (Just "player0") (words "goto town square"))
                ], []::[Message])
          assertBool "input messages are all consumed" $ null inputMsgs
          let room = fromRight $ findRoomOfPlayerExactly (Just "player0") newWorld
          assertEqual "player0 is in town square" "town square" (roomName room)
          let player = fromRight $ findCharacterInRoomExactly (Just "player0") room
          let (MsgGoto fromRoom char toRoom) = head $ List.filter isMsgGoto outputMsgs
          assertEqual "fromRoom is the Unicorn" "The Black Unicorn" (roomName fromRoom)
          assertEqual "player is Hel Mut" "Hel Mut" (charName char)
          assertEqual "toRoom is town square" "town square" (roomName toRoom)
      )
    , it "a player joins, then picks up the scroll of forgery, then forges an item"
      ( TestCase $ do
          let world2 = fromRight $ insertItemToRoom scroll1 "The Black Unicorn" world
          let (world3, (inputMsgs, outputMsgs)) = State.runState (run world2) (
                [ (MsgPlayerEnters (Just "player0") "Hel Mut")
                , (MsgCommand      (Just "player0") (words "take scroll"))
                , (MsgCommand      (Just "player0") (words "forge mug of beer $ hmmmmm, beer"))
                ], []::[Message])
          assertBool "input messages are all consumed" $ null inputMsgs
          let room = fromRight $ findRoomOfPlayerExactly (Just "player0") world3
          assertBool "scroll is not in the room" $ isLeft $ findItemInRoom "scroll of forgery" room
          let char = fromRight $ findCharacterExactly (Just "player0") world3
          assertBool "scroll is in the players inventory" $ isRight $ characterFindItem "scroll of forgery" char
          assertBool "beer is in the players inventory" $ isRight $ characterFindItem "mug of beer" char
          let (MsgForge c it) = head $ List.filter isMsgForge outputMsgs
          assertEqual "character forged something" "Hel Mut" (charName c)
          assertEqual "forged item is mug of beer" "mug of beer" (itemName it)
      )
    ]

  ]

main = hspec specs
