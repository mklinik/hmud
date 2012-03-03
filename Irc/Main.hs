{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import qualified Network.SimpleIRC as IRC
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, isJust)
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Control.Monad.State as State
import Control.Monad.State (liftIO, StateT, evalStateT)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.RealData
import Hmud.Commands
import Hmud.Message
import Hmud.Hmud

ircConfig = IRC.defaultConfig
  { IRC.cAddr = "irc.freenode.net" -- Address
  , IRC.cNick = "towerOracle"
  , IRC.cUsername = "hmudIrcBot"
  , IRC.cRealname = "hmud irc bot"
  , IRC.cChannels = ["#hmudGame"] -- Channels to join on connect
  , IRC.cEvents = [] -- Events to bind
  }

onMessage :: MVar IncomingMessage -> IRC.EventFunc
onMessage msgMVar server message
  | chan == (IRC.cNick ircConfig) = do -- only private messages to the oracle directly
      let tokens = words $ B.unpack msg
      putMVar msgMVar $ MsgCommand (B.unpack origin) tokens
  | otherwise = putStrLn $ show message
  where chan = B.unpack $ fromJust $ IRC.mChan message
        origin = fromJust $ IRC.mOrigin message
        msg = IRC.mMsg message

onJoin :: MVar IncomingMessage -> IRC.EventFunc
onJoin msgMVar server message
  | nick == (IRC.cNick ircConfig) = return () -- ignore the join message from myself
  | otherwise = putMVar msgMVar $ MsgPlayerEnters nick nick user
  where nick = B.unpack $ fromJust $ IRC.mNick message
        user = B.unpack $ fromJust $ IRC.mUser message

onNick :: MVar IncomingMessage -> IRC.EventFunc
onNick msgMVar server message
  | nick == (IRC.cNick ircConfig) = return () -- ignore message from myself
  | otherwise = do
      putStrLn $ "nick change: " ++ nick ++ " -> " ++ msg
      putMVar msgMVar $ MsgPlayerEnters msg msg user
  where nick = B.unpack $ fromJust $ IRC.mNick message
        user = B.unpack $ fromJust $ IRC.mUser message
        msg = B.unpack $ IRC.mMsg message

onPart :: MVar IncomingMessage -> IRC.EventFunc
onPart msgMVar server message =
  putMVar msgMVar $ MsgPlayerLeaves nick
    where nick = B.unpack $ fromJust $ IRC.mNick message

instance MonadHmud (StateT (IRC.MIrc, MVar IncomingMessage) IO) where
  waitForMessage = do
    (_, msgMVar) <- State.get
    debugOut "waiting for message"
    liftIO $ takeMVar msgMVar
  sendMessage addr msg = do
    (server, _) <- State.get
    liftIO $ IRC.sendMsg server (B.pack addr) (B.pack $ describeMessage addr msg)
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn
  saveGame f m = liftIO $ saveWorld f m
  loadGame f m = liftIO $ loadWorld f m

main = do
  msgMVar <- newEmptyMVar :: IO (MVar IncomingMessage)

  let events = [ (IRC.Privmsg (onMessage msgMVar))
               , (IRC.Join (onJoin msgMVar))
               , (IRC.Nick (onNick msgMVar))
               , (IRC.Part (onPart msgMVar))
               ]
  eitherIrc <- IRC.connect (ircConfig { IRC.cEvents = events }) True True

  case eitherIrc of
    Left _ -> return () -- connect failed
    Right mirc ->
        liftIO (loadWorld "save.txt" world) >>=
        \w -> evalStateT (run w) (mirc, msgMVar) >>=
        liftIO . saveWorld "save.txt"
