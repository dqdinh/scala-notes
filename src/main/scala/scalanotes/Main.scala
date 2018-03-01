package scalanotes

/**
 * Ch.8 Case Study: Testing Asynchronous Code
 */

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse

object Main extends App {
  println("Hello Cats!")
}

trait UptimeClient {
  def getUptime(hostname: String): Future[Int]
}

class UptimeService(client: UptimeClient) {
  def getTotalUptime(hostnames: List[String]): Future[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}
