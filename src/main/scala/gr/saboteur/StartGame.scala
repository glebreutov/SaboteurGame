package gr.saboteur

object StartGame extends App {
  var game = Game.start(3)
  println("Game started")
  while (true){
    DungeonGen.printMap(game.graph.graph)
    val player = game.players.head
    println("It's", player.id, " turn now")
    println(player.cards)

    val str = scala.io.StdIn.readLine()
    game = game.makeTurn(player, player.cards.head)
    println(str)
  }

}
