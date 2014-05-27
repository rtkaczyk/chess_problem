package chess.parallel

import akka.actor._
import akka.remote.RemoteScope
import com.typesafe.config._
import collection.JavaConversions._

object Deployment {
  
  val SysName = "chess"
  
  def apply(system: ActorSystem, coordinator: ActorRef): List[ActorRef] = {
    
    val port = system.settings.config.getInt("akka.remote.netty.tcp.port")
    
    def deployScope(hostname: String) = 
      RemoteScope(Address("akka.tcp", SysName, hostname, port))
    
    val perHost: List[(String, Int)] = system.settings.config
      .getConfigList("app.deployment").toList
      .map(o => o.getString("hostname") -> o.getInt("crunchers"))
    
    for (((hostname, n), i) <- perHost.zipWithIndex; j <- 0 until n)
      yield system.actorOf(Props(classOf[Cruncher], coordinator)
        .withDeploy(Deploy(scope = deployScope(hostname))), 
        s"cruncher-$i-$j")
  }
}