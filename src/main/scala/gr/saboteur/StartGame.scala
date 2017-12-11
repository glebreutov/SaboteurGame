package gr.saboteur

object StartGame extends App {
  val PLAYER_COUNT = 10
  var game = Game.start(PLAYER_COUNT)
  println("Game started")
  var i = 0
  while (!game.endOfGame()){

    val player = game.players.head
    println("It's", player.id, " turn now, tunr num:", i, "cards in deck", game.deck.length, "players count", game.players.length)
    println("player spells", player.spells)
    println("player hand", player.hand)

    //val turn = if(i % 3== 0) parseTurn(scala.io.StdIn.readLine()) else
    val turn = RoboDwarf.randomTurn(game)
    println(turn)
    game += turn
    DungeonGen.printMap(game.dungeon.graph)
    i+=1
    //game = game + turn
    //println(str)
  }

  println("END OF THE GAME!, GOLD:", game.dungeon.goldFound())
  println(game.dungeon.graph)

}
