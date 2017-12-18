#Sabateur game

This is Scala implementation of Sabateur game with rest interface. 

##Rest interface

### create new game
**url:** /createGame

create's new game and returns player id and game id
####response:
```$json
{"sessionId":"dacd28aec58a218892bd88a758d5353266cee1c5c8f89162185771282e14f037","playerId":"d671c3862b146973d9c628f2879add12ce86b2ade91d671226adaef540398bad","status":"ok"}
``` 

### join game
**url:**/join/_session_id_

allow's player to join game by session id

####response:
```$json
{"playerId":"a293fc1b5130ae09e1e54592ee9ea66d9e5245ad441b973a0d1df94556d373f5","status":"ok"}
``` 
###start game
allows game creator to start game
####response:
```$json
{"status":"ok"}
``` 
###snapshot 
**url:** /snapshot/_session_id_/_player_id_
gives full information about game state to player

####response:
```$json
{"sessionId":"dacd28aec58a218892bd88a758d5353266cee1c5c8f89162185771282e14f037","playerID":"d671c3862b146973d9c628f2879add12ce86b2ade91d671226adaef540398bad","turns":0,"goldFound":false,"gameEnd":false,"hand":["12-DUNGEON$@╬","7-DUNGEON$@║","39-DUNGEON$@╔","52-DEADEND$@┬","81-FIX_TRUCK$","99-FIX_LANTERN_TRUCK$"],"spells":[],"map":[[{"row":6,"col":0},"1-GOLD$@*"],[{"row":6,"col":-2},"0-ORE$@╬"],[{"row":6,"col":2},"0-ORE$@╬"],[{"row":0,"col":0},"2-START$@╦"]],"revelations":[]}
``` 

###make turn - pass
allows player to pass turn
** url: ** /pass-turn/sessionId/playerID/cardId

###make turn - victim
allows player to cast a spell to other player
** url: ** /victim-turn/sessionId/playerID/cardId/otherPlayerId

###make turn - map
allows player to make map related turn
** url: ** /map-turn/sessionId/playerID/cardId/location

location is pair of digits divided by @. for example 1@-3
