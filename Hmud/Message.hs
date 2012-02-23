module Hmud.Message where

import Hmud.Character
import Hmud.Room
import Hmud.Item

-- * info messages are sent to the player only
-- * goto messages are sent to:
--  * all players of the old room (someone leaves)
--  * all players of the new room (someone enters)
-- * take, put and give messages are sent to all players of the current room
data Message =
    MsgInfo String
  | MsgGoto Room Character Room
  | MsgTake Character Item
  | MsgPut Character Item
  | MsgGive Character Item Character
  | MsgForge Character Item
  deriving (Show, Eq)

data IncomingMessage =
    MsgCommand Address [String]
  | MsgPlayerEnters Address String
  | MsgExit
  deriving (Show, Eq)

isMsgGoto (MsgGoto _ _ _) = True
isMsgGoto _ = False

isMsgTake (MsgTake _ _) = True
isMsgTake _ = False

isMsgForge (MsgForge _ _) = True
isMsgForge _ = False


-- $ "The world around you gets dark. All sounds seem to fade. A moment of complete darkness is followed by a bright flash. As you slowly open your eyes again, a brand new " ++ itName ++ " hovers in the air before you, then floats slowly into your hands."
