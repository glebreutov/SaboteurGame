package gr.saboteur.rest

import gr.saboteur.DungeonGraph.Dot
import gr.saboteur._
import gr.saboteur.rest.SabateurWeb.{PlayerID, SessionId}
import gr.saboteur.rest.UID.sha256Hash
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.dsl._
import io.circe.syntax._
import io.circe.generic.auto._

import scala.collection.mutable

object UID {
  def sha256Hash(text: String) : String = String.format("%064x", new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8"))))

  //fix it: add some random here
  def getGameId() : SessionId = sha256Hash(System.currentTimeMillis().toString)

  def getPlayerId() : PlayerID = sha256Hash(System.currentTimeMillis().toString + "player")
}
case class JoinMsg(sessionId: SessionId, playerID: PlayerID)
case class Pass(cardId: Int)
case class JPlayer(id: PlayerID, spells: Set[Int])
case class GameStatus(sessionId: SessionId, playerID: PlayerID,
                      turns: Int, goldFound: Boolean, gameEnd: Boolean,
                      hand: Set[String], spells: Set[String], map: List[(Dot, String)], revelations: Set[(Dot, String)])
object SabateurWeb {

  type PlayerID = String
  type SessionId = String

  var lobby: mutable.Map[SessionId, Set[PlayerID]] = mutable.Map()
  var games: mutable.Map[SessionId, Game] = mutable.Map()

  val service = HttpService {
    case GET -> Root / "createGame" => Ok(createGame())
    case GET -> Root / "join" / sessionId => Ok(join(sessionId))
    case GET -> Root / "start" / sessionId / playerID => Ok(startGame(sessionId, playerID))
    case GET -> Root / "snapshot" / sessionId / playerID => Ok(gameSanpshot(sessionId, playerID))
    case GET -> Root / "pass-turn" / sessionId / playerID / cardId => {
      val json = maybeCard(cardId).map(c => makeTurn(sessionId, playerID, PlayersTurn(c)))
      Ok(json.getOrElse(responseErr("Invalid card id")))
    }
    case GET -> Root / "victim-turn" / sessionId / playerID / cardId / otherPlayerId => {
      val turnRes = maybeCard(cardId)
        .flatMap(c => maybePlayer(sessionId, otherPlayerId).map(p => PlayersTurn(c, victim = p)))
        .map(t => makeTurn(sessionId, playerID, t))
      Ok(turnRes.getOrElse(responseErr("Invalid card id or player id")))
    }
    case GET -> Root / "map-turn" / sessionId / playerID / cardId / point => {
      val turnRes = maybeCard(cardId).flatMap(c => maybePoint(point).map(d => PlayersTurn(c, location = d)))
        .map(t => makeTurn(sessionId, playerID, t))
      Ok(turnRes.getOrElse(responseErr("Invalid card id or dot")))
    }
  }

  def maybePoint(point: String): Option[Dot] = {
    def isAllDigits(x: String) = x forall Character.isDigit

    val strings = point.split("@").filter(isAllDigits).map(_.toInt)
    if(strings.length == 2){
      Some(Dot(strings(0), strings(1)))
    }else {
      None
    }
  }
  def maybePlayer(sessionId: SessionId, id: PlayerID): Option[Player] = {
    games.get(sessionId).flatMap(g => g.players.find(_.id == id))
  }

  def maybeCard(id: String): Option[Card] = {
    Cards.deck.find(_.id == id)
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
    else {
      val game = games(sessionId)
      val player = game.players.filter(_.id == playerID).head
      val hand = player.hand.map(_.toString)
      val spells = player.spells.map(_.toString)
      val dungeon = game.dungeon.graph.map(e => e._1 -> e._2.toString).toList
      val revelations = player.revelations.map(dot => dot-> game.dungeon.graph(dot).toString)
      GameStatus(sessionId, playerID, game.turns.size, game.goldFound(), game.endOfGame(), hand, spells, dungeon, revelations).asJson
    }

  }

  def makeTurn(sessionId: SessionId, playerID: PlayerID, turn: PlayersTurn): Json = {
    lazy val game = games(sessionId)
    lazy val (updated, message) = game makeTurn turn
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
    }else if(!game.legitTurn(turn)){
      responseErr(message)
    }else {
      games(sessionId) = updated
      gameSanpshot(sessionId, playerID)
    }
  }

}
