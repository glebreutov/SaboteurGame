package gr.saboteur

import gr.saboteur.StartGame.game

object StartGame extends App {
  var game = Game.start(3)
  println("Game started")
  while (!game.endOfGame()){
    DungeonGen.printMap(game.dungeon.graph)
    val player = game.players.head
    println("It's", player.id, " turn now")
    println("player spells", player.spells)
    println("player hand", player.hand)

    val turn = parseTurn(scala.io.StdIn.readLine())
    game = turn match {
      case tun : MakeTunnel => game + tun
      case tun : CastSpell => game + tun
      case tun : RevealTreasure => game + tun
      case tun : DestroyTunnel => game + tun
      case tun : Pass => game + tun
    }
    //game = game + turn
    //println(str)
  }

  def parseTurn[T <: Turn](s: String): T = {
    //card num + action + (pos | player id)
    //1 tunnel (1, 0)
    //3 spell Dwalli
    //6 pass
    throw new RuntimeException("Implement me")
  }

}
