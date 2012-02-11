A simple MUD
============

where you can't do much.

Written in Haskell.

It's intended to jazz up XMPP multi user chat rooms with some funny stories.
Every participant in the group chat is assigned a randomly generated character.
People can move to different rooms, pick up items and give items to each other.
Nothing more and nothing less. Maybe some funny situations arise if the game
provides odd characters, locations and items.

- People chat with each other using the group chat, like without the game.
- The game is a participant in the room, too. It's called "oracle"
- Every time a new user enters the room, the oracle greets the user in public:
  "Welcome Markus, you are a level 44 Female Elven Servant."
- To enter commands to the game, players open a private conversation with the oracle
- If someone gives an item to a player, the oracle sends the player a private
  message
- There are no NPCs
- There are rooms, items and characters
- Every room, item and character has a description
- The description of items is static
- The description of rooms is:
  - a static text
  - the list characters
  - the list of items in the room
  - list of possible exits
- The description of characters depends on equipment and stats
- Every player has:
  - stats
  - an inventory (with objects)
  - equipped objects
- Stats are:
  - level
  - gender
  - race (Human, Elven, Dwarf)
  - role (Knight, Wizard, King, etc)

Commands
========

Commands and names of things can be abbreviated. If you are in a room where another room named "Forest" is adjacent, the commands

    goto Forest

and

    go For

do the same thing.

- lookat *thing*
 - without argument, look at the current room
 - with argument: print the description of rooms, items or characters
- goto *room-name*
 - go to a different room. Only possible if *room-name* is adjacent to the
   current room
- pickup
 - pickup an item from the current room
- put
 - put an item in the current room
- give *item* to *character*
 - give an item from your inventory to another character
