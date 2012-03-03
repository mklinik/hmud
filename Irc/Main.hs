{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import qualified Network.SimpleIRC as IRC
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (fromJust, isJust)
import           Control.Concurrent.Chan
import           Control.Concurrent (forkIO)
import qualified Control.Monad.State as State
import           Control.Monad.State (liftIO, StateT, evalStateT)

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

onMessage :: Chan IncomingMessage -> IRC.EventFunc
onMessage msgChan server message
  | chan == (IRC.cNick ircConfig) = do -- only private messages to the oracle directly
      let tokens = words $ B.unpack msg
      writeChan msgChan $ MsgCommand (B.unpack origin) tokens
  | otherwise = putStrLn $ show message
  where chan = B.unpack $ fromJust $ IRC.mChan message
        origin = fromJust $ IRC.mOrigin message
        msg = IRC.mMsg message

onJoin :: Chan IncomingMessage -> IRC.EventFunc
onJoin msgChan server message
  | nick == (IRC.cNick ircConfig) = return () -- ignore the join message from myself
  | otherwise = writeChan msgChan $ MsgPlayerEnters nick nick user
  where nick = B.unpack $ fromJust $ IRC.mNick message
        user = B.unpack $ fromJust $ IRC.mUser message

onNick :: Chan IncomingMessage -> IRC.EventFunc
onNick msgChan server message
  | nick == (IRC.cNick ircConfig) = return () -- ignore message from myself
  | otherwise = do
      putStrLn $ "nick change: " ++ nick ++ " -> " ++ msg
      writeChan msgChan $ MsgPlayerEnters msg msg user
  where nick = B.unpack $ fromJust $ IRC.mNick message
        user = B.unpack $ fromJust $ IRC.mUser message
        msg = B.unpack $ IRC.mMsg message

onPart :: Chan IncomingMessage -> IRC.EventFunc
onPart msgChan server message =
  writeChan msgChan $ MsgPlayerLeaves nick
    where nick = B.unpack $ fromJust $ IRC.mNick message

instance MonadHmud (StateT (IRC.MIrc, Chan IncomingMessage) IO) where
  waitForMessage = do
    (_, msgChan) <- State.get
    debugOut "waiting for message"
    liftIO $ readChan msgChan
  sendMessage addr msg = do
    (server, _) <- State.get
    liftIO $ IRC.sendMsg server (B.pack addr) (B.pack $ describeMessage addr msg)
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn
  saveGame f m = liftIO $ saveWorld f m
  loadGame f m = liftIO $ loadWorld f m

main = do
  msgChan <- newChan :: IO (Chan IncomingMessage)

  let events = [ (IRC.Privmsg (onMessage msgChan))
               , (IRC.Join (onJoin msgChan))
               , (IRC.Nick (onNick msgChan))
               , (IRC.Part (onPart msgChan))
               ]
  eitherIrc <- IRC.connect (ircConfig { IRC.cEvents = events }) True True

  case eitherIrc of
    Left _ -> return () -- connect failed
    Right mirc ->
        liftIO (loadWorld "save.txt" world) >>=
        \w -> evalStateT (run w) (mirc, msgChan) >>=
        liftIO . saveWorld "save.txt"
