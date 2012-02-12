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
- The program is divided in two parts: the game engine, and the XMPP frontend
- The XMPP frontend must ensure that player names are unique
- To use XMPP grou chats, players must already have a jabber account, so we
  don't need to care about logins etc.
- Just map a character to a jabber ID somehow

Screenshot
==========

The XMPP frontend does not yet exist. You interact with the game using ghci, no readline.

    $ ghci Main.hs
    *Main> main
    >>> look
    This is The Black Unicorn, a dusty dark tavern. It smells of delicious food. You hear cheery background music.

    These people are present:
    Karin, Martin, Markus

    These things lie about:
    mug of beer

    From here you can go to:
    town square
    >>> lookat Markus
    You see Markus, a level 7 male elven Merchant
    >>> l mug
    You see mug of beer, filled with fresh, foaming, delicious beer.
    >>> take mug
    You take mug of beer
    >>> inventory
    Your possessions: mug of beer
    >>> look mug
    You see mug of beer, filled with fresh, foaming, delicious beer.
    >>> goto town
    You are now in town square, the central meeting place of the town. There is a fountain, trees and flowers, and lots of people that are busy with their daily routine. The sun shines, birds sing and everybody is quite happy.

    These people are present:
    Markus, Kathy

    From here you can go to:
    The Black Unicorn, ivory tower
    >>>

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
- take *item-name*
 - pickup an item from the current room
- put *item-name*
 - put an item from your inventory in the current room
- give *item* to *character*
 - give an item from your inventory to another character
