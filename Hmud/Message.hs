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
  | MsgGoto Character Room
  | MsgTake Character Item
  | MsgPut Character Item
  | MsgGive Character Item Character
