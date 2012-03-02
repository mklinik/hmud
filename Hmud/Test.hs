module Hmud.Test where

import Test.Hspec.HUnit
import Test.Hspec
import Test.HUnit
import Data.Maybe (fromJust, isNothing, isJust)
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

emptyWorld = World { worldRooms = [], idleCharacters = [] }

specs :: Specs
specs = descriptions
  [ describe "insertCharacterToRoom"
    [ it "returns Nothing when there is no such room in the world"
        (either (const True) (const False) $ insertCharacterToRoom player0 "what where?" emptyWorld)
    , it "returns Nothing when there is no such room in the world"
        (either (const True) (const False) $ insertCharacterToRoom player0 "what where?" world)
    , it "returns the world where the player is in the desired room on success"
        ((do
          w <- insertCharacterToRoom player0 "Black Unicorn" world
          r <- findRoom "Black Unicorn" w
          findCharacterInRoom "player0" r
        ) == Right player0)
    , it "works with abbreviated room name"
        ((do
          w <- insertCharacterToRoom player0 "Bl" world
          r <- findRoom "Black Unicorn" w
          findCharacterInRoom "player0" r
        ) == Right player0)
    ]

  , describe "gotoFromTo"
    [ it "returns Nothing if there is no such *from* room"
        (either (const True) (const False) $ gotoFromTo "player0addr" "what where?" "Black Unicorn" world)
    , it "returns Nothing if there is no such *to* room"
        (either (const True) (const False) $ gotoFromTo "player0addr" "Black Unicorn" "what where?" world)
    , it "returns Nothing if there is no such character in the *from* room"
        (either (const True) (const False) $ gotoFromTo "slayer0" "Black Unicorn" "town square" world)
    , it "works when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "Black Unicorn" world
                  let (w3, _, _, _) = fromRight $ gotoFromTo "player0addr" "Black Unicorn" "town square" w2
                  let fromRoom = fromRight $ findRoom "Black Unicorn" w3
                  let toRoom   = fromRight $ findRoom "town square" w3
                  assertBool "player is no longer in *fromRoom*" $ isLeft (findCharacterInRoom "player0" fromRoom)
                  assertEqual "player is now in *toRoom*" (Right player0) (findCharacterInRoom "player0" toRoom)
      )
    , it "does not work with abbreviated names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "Black" world
                  assertBool "no such player pl" $ isLeft $ gotoFromTo "pl" "Th" "to" w2
      )
    , it "fails when trying to go to the same room again"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "Black Unicorn" world
                  assertBool "going to the same room fails" $ isLeft $ gotoFromTo "player0addr" "Black" "Black Un" w2
      )
    ]
  , describe "findCharacterByAddress"
    [ it "succeeds when everything is fine"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "Black Unicorn" world
                  assertEqual "player found" (Right player0) (findCharacterByAddress "player0addr" w2)
                  assertBool "player not found" $ isLeft (findCharacterByAddress "slayer0" w2)
      )
    , it "fails in the empty world"
      (TestCase $ do
                  assertBool "player not found" $ isLeft (findCharacterByAddress "player0addr" emptyWorld)
                  assertBool "player not found" $ isLeft (findCharacterByAddress "slayer0" emptyWorld)
      )
    , it "fails with partial names"
      (TestCase $ do
                  let w2 = fromRight $ insertCharacterToRoom player0 "Black Unicorn" world
                  assertBool "player not found" $ isLeft (findCharacterByAddress "pla" w2)
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
    [ it "a player joins, and is put to Black Unicorn"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [(MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          assertEqual "player0 is in the Unicorn"
            "Black Unicorn" (roomName (fromRight $ findRoomOfPlayerByAddress "player0" newWorld))
      )
    , it "a player joins, then goes to town square"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgCommand      "player0" (words "goto town square"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let room = fromRight $ findRoomOfPlayerByAddress "player0" newWorld
          assertEqual "player0 is in town square" "town square" (roomName room)
          let player = fromRight $ findCharacterInRoomByAddress "player0" room
          let (_, MsgGoto fromRoom char toRoom) = head $ List.filter (isMsgGoto . snd) outputMsgs
          assertEqual "fromRoom is the Unicorn" "Black Unicorn" (roomName fromRoom)
          assertEqual "player is Hel Mut" "Hel Mut" (charName char)
          assertEqual "toRoom is town square" "town square" (roomName toRoom)
      )
    , it "a player joins, then picks up the scroll of forgery, then forges an item"
      ( TestCase $ do
          let world2 = fromRight $ insertItemToRoom scroll1 "Black Unicorn" world
          let (world3, (inputMsgs, outputMsgs, _)) = State.runState (run world2) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgCommand      "player0" (words "take scroll"))
                , (MsgCommand      "player0" (words "forge mug of beer $ hmmmmm, beer"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          assertEqual "we got exactly one take message" 1 (length $ List.filter (isMsgTake . snd) outputMsgs)
          let room = fromRight $ findRoomOfPlayerByAddress "player0" world3
          assertBool "scroll is not in the room" $ isLeft $ findItemInRoom "scroll of forgery" room
          let char = fromRight $ findCharacterByAddress "player0" world3
          assertBool "scroll is in the players inventory" $ isRight $ characterFindItem "scroll of forgery" char
          assertBool "beer is in the players inventory" $ isRight $ characterFindItem "mug of beer" char
          let (_, MsgForge c it) = head $ List.filter (isMsgForge . snd) outputMsgs
          assertEqual "character forged something" "Hel Mut" (charName c)
          assertEqual "forged item is mug of beer" "mug of beer" (itemName it)
      )
    , it "three players join, one picks up the scroll and drops the scroll again"
      ( TestCase $ do
          let world2 = fromRight $ insertItemToRoom scroll1 "Black Unicorn" world
          let (world3, (inputMsgs, outputMsgs, _)) = State.runState (run world2) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "player1" "Ara Gorn" "ara.gorn@localhost")
                , (MsgPlayerEnters "player2" "Bil Bo" "bil.bo@localhost")
                , (MsgCommand      "player0" (words "take scroll"))
                , (MsgCommand      "player0" (words "put scroll"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let takeMsgs = List.filter (isMsgTake . snd) outputMsgs
          let putMsgs = List.filter (isMsgPut . snd) outputMsgs
          assertEqual "we got three take messages" 3 (length takeMsgs)
          assertEqual "we got three put messages" 3 (length putMsgs)
          assertBool "one take message is to player0" $ isJust $ List.find (\(addr, _) -> addr == "player0") takeMsgs
          assertBool "one take message is to player1" $ isJust $ List.find (\(addr, _) -> addr == "player1") takeMsgs
          assertBool "one take message is to player2" $ isJust $ List.find (\(addr, _) -> addr == "player2") takeMsgs
          assertBool "one put message is to player0" $ isJust $ List.find (\(addr, _) -> addr == "player0") putMsgs
          assertBool "one put message is to player1" $ isJust $ List.find (\(addr, _) -> addr == "player1") putMsgs
          assertBool "one put message is to player2" $ isJust $ List.find (\(addr, _) -> addr == "player2") putMsgs
      )
    , it "two players A and B enter. B exits and leaves its character behind. A changes it's nick to the nick of B, but must still control A's character."
      ( TestCase $ do
          let (world2, (inputMsgs, outputMsgs, debugs)) = State.runState (run world) (
                [ (MsgPlayerEnters "playerA" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "playerB" "Ara Gorn" "ara.gorn@localhost")
                -- now, Ara Gorn leaves and playerA tries to impersonate playerB
                , (MsgPlayerEnters "playerB" "Hel Mut" "hel.mut@localhost")
                , (MsgCommand      "playerB" (words "goto town"))
                ], []::[TestStateOutgoing], []::[String])
          mapM_ (hPutStrLn stderr) debugs
          let unicorn = fromRight $ findRoom "Black Unicorn" world2
          let townSqr = fromRight $ findRoom "town square" world2
          assertBool "Ara Gorn is still in The Unicorn" $ isRight $ findCharacterInRoom "Ara Gorn" unicorn
          assertBool "Hel Mut went to town square" $ isRight $ findCharacterInRoom "Hel Mut" townSqr
      )
    ]

  , describe "say"
    [ it "can only be heard in the current room"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "player1" "Ara Gorn" "ara.gorn@localhost")
                , (MsgPlayerEnters "player2" "Bil Bo" "bil.bo@localhost")
                , (MsgCommand      "player0" (words "goto town"))
                , (MsgCommand      "player1" (words "say hello"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let sayMsgs = List.filter (isMsgSay . snd) outputMsgs
          assertEqual "we got 2 say messages" 2 $ length sayMsgs
          assertBool "one say message is to player1" $ isJust $ List.find (\(addr, _) -> addr == "player1") sayMsgs
          assertBool "one say message is to player2" $ isJust $ List.find (\(addr, _) -> addr == "player2") sayMsgs
      )
    ]

  , describe "tell"
    [ it "can only be heard by the receipient and the speaker"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "player1" "Ara Gorn" "ara.gorn@localhost")
                , (MsgPlayerEnters "player2" "Bil Bo" "bil.bo@localhost")
                , (MsgCommand      "player0" (words "goto town"))
                , (MsgCommand      "player1" (words "tell Bil $ hello"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let tellMsgs = List.filter (isMsgTell . snd) outputMsgs
          assertEqual "we got 2 tell messages" 2 $ length tellMsgs
          assertBool "one say message is to player2" $ isJust $ List.find (\(addr, _) -> addr == "player2") tellMsgs
          assertBool "one say message is to player1" $ isJust $ List.find (\(addr, _) -> addr == "player1") tellMsgs
      )
    ]

  , describe "MsgPlayerLeaves"
    [ it "puts the leaving player from the room in the idle list"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerLeaves "player0")
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let unicorn = fromRight $ findRoom "Black Unicorn" newWorld
          assertBool "Black Unicorn is empty" $ null $ roomCharacters unicorn
          assertBool "Hel Mut is in the idle list" $ isJust $ List.find (\c -> charAddress c == "player0") (idleCharacters newWorld)
      )
    , it "inserts a player back to the game when the player enters again"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerLeaves "player0")
                , (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let unicorn = fromRight $ findRoom "Black Unicorn" newWorld
          assertEqual "Black Unicorn is not empty" 1 (length $ roomCharacters unicorn)
          assertBool "the idle list is empty" $ null $ (idleCharacters newWorld)
      )
    , it "other players can not speak to a left player"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "player1" "Ara Gorn" "ara.gorn@localhost")
                , (MsgPlayerLeaves "player0")
                , (MsgCommand      "player1" (words "tell Hel Mut $ hello"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let tellMsgs = List.filter (isMsgTell . snd) outputMsgs
          assertBool "we got no tell messages" $ null tellMsgs
      )
    , it "when a player enters again, everything is back to normal"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "player1" "Ara Gorn" "ara.gorn@localhost")
                , (MsgPlayerLeaves "player0")
                , (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgCommand      "player1" (words "tell Hel Mut $ hello"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let tellMsgs = List.filter (isMsgTell . snd) outputMsgs
          assertEqual "we got 2 tell messages" 2 $ length tellMsgs
          assertBool "one tell message is to player0" $ isJust $ List.find (\(addr, _) -> addr == "player0") tellMsgs
          assertBool "one tell message is to player1" $ isJust $ List.find (\(addr, _) -> addr == "player1") tellMsgs
          assertBool "the idle list is empty" $ null $ (idleCharacters newWorld)
      )
    , it "when a player enters again but with a different address, everything works just fine"
      ( TestCase $ do
          let (newWorld, (inputMsgs, outputMsgs, _)) = State.runState (run world) (
                [ (MsgPlayerEnters "player0" "Hel Mut" "hel.mut@localhost")
                , (MsgPlayerEnters "player1" "Ara Gorn" "ara.gorn@localhost")
                , (MsgPlayerLeaves "player0")
                , (MsgPlayerEnters "player2" "Hel Mut" "hel.mut@localhost")
                , (MsgCommand      "player1" (words "tell Hel Mut $ hello"))
                ], []::[TestStateOutgoing], []::[String])
          assertBool "input messages are all consumed" $ null inputMsgs
          let tellMsgs = List.filter (isMsgTell . snd) outputMsgs
          assertEqual "we got 2 tell messages" 2 $ length tellMsgs
          assertBool "one tell message is to player2" $ isJust $ List.find (\(addr, _) -> addr == "player2") tellMsgs
          assertBool "one tell message is to player1" $ isJust $ List.find (\(addr, _) -> addr == "player1") tellMsgs
          assertBool "the idle list is empty" $ null $ (idleCharacters newWorld)
      )
    ]

  ]

main = hspec specs
