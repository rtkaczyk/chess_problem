package chess.parallel

import akka.actor._
import collection.mutable
import Protocol._
import chess.common.Domain._
import chess.common.IO
import scala.util.Random
import com.typesafe.config._
import collection.JavaConversions._

class Coordinator extends Actor with ActorLogging {
  
  val solutions = mutable.ListBuffer[Map[Square, Piece]]()
  
  val crunchers = context.system.settings.config
    .getObject("akka.actor.deployment").keySet().toList
    .filter(_ startsWith "/cruncher").map(_ drop 1)
    
  println("KURWA")
  println(crunchers)
  
  val idle = mutable.Set[ActorRef](
      (for (cr <- crunchers) 
        yield context.system.actorOf(Props(classOf[Cruncher], self), s"$cr")): _*)
  
  val active = mutable.Set[ActorRef]()
  
  val stats = mutable.Map[String, Int]() withDefaultValue 0
  
  val unhandledFrames = mutable.Queue[Frame]()
  
  def receive = {
    case Finished(sols) =>
      solutions ++= sols
      val cruncher = sender()
      //log.info("Finished: " + cruncher.path.name)
      //log.info("State: " + crunchersState)
      markIdle(cruncher)
      stats(cruncher.path.name) = stats(cruncher.path.name) + 1 
      
      if (unhandledFrames.nonEmpty)
        sender() ! unhandledFrames.dequeue()
      else if (active.nonEmpty)
        selectCruncher(active) ! Share
      else {
        idle foreach (_ ! PoisonPill)
        IO.output(solutions)
        println("\nSTATS:")
        println(stats.toList.sortBy(_._1).map(x => s"${x._1} -> ${x._2}").mkString("\n"))
        context.system.shutdown()
      }
    
    case f: Frame =>
      if (idle.isEmpty)
        unhandledFrames.enqueue(f)
      else {
        val cruncher = selectCruncher(idle)
        //log.info("Received a frame from: %s. Sending to: %s".format(sender().path.name, cruncher.path.name))
        cruncher ! f
        markActive(cruncher)
        //log.info("State: " + crunchersState)
        if (idle.nonEmpty) {
          //log.info("Sending Share request to: " + cruncher.path.name)
          cruncher ! Share
        }
      }
  }
  
  def markIdle(actor: ActorRef) {
    active -= actor
    idle += actor
  }
  
  def markActive(actor: ActorRef) {
    idle -= actor
    active += actor
  }
  
  def selectCruncher(set: mutable.Set[ActorRef]): ActorRef = {
    val i = Random.nextInt(set.size)
    set.toList(i)
  }
  
  def crunchersState = 
    s"Active: [${active.map(_.path.name).mkString(", ")}], " +
    s"Idle: [${idle.map(_.path.name).mkString(", ")}] "
}