package messaging

import akka.actor.{ActorRef, ActorSystem, Props}
import com.spingo.op_rabbit._
import com.spingo.op_rabbit.properties.ReplyTo
import communication.MessageFormat.MyFormat
import communication._

import scala.concurrent.ExecutionContext.Implicits.global

/** Manages casual matchmaking request and response messages. */
object MatchmakingManager {
  private val rabbitControl: ActorRef = ActorSystem().actorOf(Props[RabbitControl])
  implicit private val recoveryStrategy: RecoveryStrategy = RecoveryStrategy.nack(false)

  import Queues._
  private val joinCasualMatchmakingRequestQueue =
    Queue(JoinCasualMatchmakingRequestQueue, durable = false, autoDelete = true)
  private val joinCasualMatchmakingResponseQueue =
    Queue(JoinCasualMatchmakingResponseQueue, durable = false, autoDelete = true)

  implicit private val RequestFormat: MyFormat[JoinCasualQueueRequest] = MessageFormat.format[JoinCasualQueueRequest]
  implicit private val ResponseFormat: MyFormat[JoinCasualQueueResponse] = MessageFormat.format[JoinCasualQueueResponse]

  private val publisher: Publisher = Publisher.queue(joinCasualMatchmakingRequestQueue)

  /** Manages casual matchmaking response messages. */
  Subscription.run(rabbitControl) {
    import Directives._
    channel(qos = 3) {
      consume(joinCasualMatchmakingResponseQueue) {
        body(as[JoinCasualQueueResponse]) { response =>
          response.opponentData match {
            case Right((opponentName, opponentTeam)) =>
              println(opponentName)
              opponentTeam.foreach(println(_))
            case Left(details) => Unit
          }
          ack
        }
      }
    }
  }

  /** Send a casual matchmaking request message.
    *
    * @param playerName username of the player that wants to join.
    * @param team team with which the player wants to fight.
    */
  def joinCasualQueueRequest(playerName: String, team: Map[String, String]): Unit = {
    val teamSeq = team.map(member => member._2).toSeq
    rabbitControl ! Message(JoinCasualQueueRequest(playerName, teamSeq, "Add"),
                            publisher,
                            Seq(ReplyTo(joinCasualMatchmakingResponseQueue.queueName)))
  }

  def leaveCasualQueueRequest(playerName: String): Unit = {
    rabbitControl ! Message(JoinCasualQueueRequest(playerName, Seq(), "Remove"),
                            publisher,
                            Seq(ReplyTo(joinCasualMatchmakingResponseQueue.queueName)))
  }
}
