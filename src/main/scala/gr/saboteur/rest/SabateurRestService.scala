package gr.saboteur.rest

import gr.saboteur.{Game, PlayersTurn}
import gr.saboteur.rest.HelloWorld.{PlayerID, SessionId}
import gr.saboteur.rest.UID.sha256Hash
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.dsl._

import scala.collection.mutable

object UID {
  def sha256Hash(text: String) : String = String.format("%064x", new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8"))))

  //fix it: add some random here
  def getGameId() : SessionId = sha256Hash(System.currentTimeMillis().toString)

  def getPlayerId() : PlayerID = sha256Hash(System.currentTimeMillis().toString + "player")
}
case class JoinMsg(sessionId: SessionId, playerID: PlayerID)
object HelloWorld {

  type PlayerID = String
  type SessionId = String

  var lobby: mutable.Map[SessionId, Set[PlayerID]] = mutable.Map()
  var games: mutable.Map[SessionId, Game] = mutable.Map()

  val service = HttpService {
    case GET -> Root / "createGame" => Ok(createGame())
    case GET -> Root / "join" / sessionId => Ok(join(sessionId))
    case GET -> Root / "start" / sessionId / playerID => Ok(startGame(sessionId, playerID))
    case GET -> Root / "snapshot" / sessionId / playerID => Ok(gameSanpshot(sessionId, playerID))
    case GET -> Root / "make_turn" / sessionId / playerID / turn => Ok(gameSanpshot(sessionId, playerID))
  }

  def responseOk(fields: (String, String) *): Json = {
    val tuples = (fields :+ ("status"-> "ok")).map(t => t._1 -> Json.fromString(t._2))
    Json.obj(tuples :_*)
  }
  def responseErr(descr: String): Json = {
    Json.obj("status"-> Json.fromString(descr))
  }
  def createGame() : Json = {
    val sessionId = UID.getGameId()
    val playerId = UID.getPlayerId()
    lobby(sessionId)  = Set(playerId)

    responseOk("sessionId" -> sessionId, "playerId" -> playerId)
  }

  def startGame(sessionId: SessionId, playerID: PlayerID) : Json = {
    if(!lobby.contains(sessionId)){
      responseErr("Invalid session id")
    }else if(lobby(sessionId).head != playerID){
      responseErr("Invalid player id")
    }else if(playersCount(sessionId) == 0){
      responseErr(s"Cant start game with only ${playersCount(sessionId)} players")
    }else if(isGameStarted(sessionId)){
      responseErr("Game already started")
    }else {
      games(sessionId) = Game.start(lobby(sessionId).toList)
      responseOk()
    }
  }

  private def playersCount(sessionId: SessionId): Int = {
    if(lobby.contains(sessionId)) lobby(sessionId).size else 0
  }

  private def isGameStarted(sessionId: SessionId) = {
    games.contains(sessionId)
  }

  val MAX_PLAYERS_PER_GAME = 6

  def join(sessionId: SessionId) : Json = {
    if(!lobby.contains(sessionId))
      responseErr("Invalid session id")
    else if(isGameStarted(sessionId)){
      responseErr("Game already started")
    }else if(playersCount(sessionId) >= MAX_PLAYERS_PER_GAME){
      responseErr("Can't add more players to game")
    }else {
      val playerID = UID.getPlayerId()
      lobby(sessionId)  = lobby(sessionId) + playerID
      responseOk("playerId"-> playerID)
    }
  }

  def gameSanpshot(sessionId: SessionId, playerID: PlayerID): Json = {
    if(!games.contains(sessionId))
      responseErr("Invalid session id")
    else
      responseErr("method not implemented")
  }

  def makeTurn(sessionId: SessionId, playerID: PlayerID, turn: PlayersTurn): Json = {
    lazy val game = games(sessionId)
    lazy val updated = game + turn
    if(!games.contains(sessionId))
      responseErr("Invalid session id")
    else if(!lobby.contains(playerID))
      responseErr("Invalid player id")
    else if(!isGameStarted(sessionId))
      responseErr("Game not started")
    else if(game.players.head.id != playerID)
      responseErr("It's not your turn")
    else if(game.endOfGame()){
      val result: String = if (game.goldFound()) "Dwarfs won" else "Sabateurs won"
      responseErr("Game already ended " + result)
    }else if(updated == game){
      responseErr("You can't play that way")
    }else {
      games(sessionId) = updated
      gameSanpshot(sessionId, playerID)
    }
  }

}
