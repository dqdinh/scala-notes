package scalanotes

/**
 * Ch.8 Case Study: Testing Asynchronous Code
 */

//import scala.concurrent.Future
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

//import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse
import cats.syntax.functor._ // for map
import cats.{ Id, Applicative }

object Main extends App {
  // F[_] is a type constructor that we can bind to Future or Id
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
}

