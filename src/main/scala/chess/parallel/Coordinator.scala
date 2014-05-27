package chess.parallel

import akka.actor._
import collection.mutable
import Protocol._
import chess.common.Domain._
import chess.common.IO
import scala.util.Random

class Coordinator extends Actor {
  
  var solutions = List[Map[Square, Piece]]()
  
  val idle = mutable.Set[ActorRef](Deployment(context.system, self): _*)
  val active = mutable.Set[ActorRef]()
  
  val unhandledFrames = mutable.Queue[Frame]()
  
  def receive = {
    case Finished(sols) =>
      solutions :::= sols
      markIdle(sender())
      
      if (unhandledFrames.nonEmpty)
        sender() ! unhandledFrames.dequeue()
      else if (active.nonEmpty)
        selectCruncher(active) ! Share
      else {
        idle foreach (_ ! PoisonPill)
        IO.output(solutions)
        context.system.shutdown()
      }
    
    case f: Frame =>
      if (idle.isEmpty)
        unhandledFrames.enqueue(f)
      else {
        val cruncher = selectCruncher(idle)
        cruncher ! f
        markActive(cruncher)
        if (idle.nonEmpty)
          cruncher ! Share
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
}