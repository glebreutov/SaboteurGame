package gr.saboteur

object Start extends App {

  override def main(args: Array[String]): Unit = {
    val game = Game.start(6)
    println(game.deck)
    println(game.players)
  }
}
