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

object Ch8TestingAsyncCode {
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

/**
 * Ch.9 Case Study: Map Reduce
 */

import cats.{ Monoid }
import cats.instances.int._ // for Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.semigroup._ // for |+|

object Ch9Notes {
  // FYI: Cat's TraversableOnce trait:
  // A template trait for collections which can be traversed either once only
  // or one or more times.
  //
  // foldLeft, foldRight, and associated methods:
  //

  // Q: how does foreach work in this trait?
  //
  // def foreach[U](f: A => U): Unit

  // Q: how does self work in this trait?
  // Q: what is ::= ?
  //
  // protected[this] def reversed = {
  //   var elems: List[A] = Nil
  //   self foreach (elems ::= _)
  //   elems
  // }

  // def foldRight[B](z: B)(op: (A, B) => B): B =
  //   reversed.foldLeft(z)((x, y) => op(y, x))

  // def foldLeft[B](z: B)(op: (B, A) => B): B = {
  //   var result = z
  //   this foreach (x => result = op(result, x))
  //   result
  // }

}

object Main extends App {

  /**
   * Single-threaded map-reduce function.
   * Maps `f` over `sequence` and reduces using a `Monoid[B]`.
   *
   * Parallel fold will yield the correct results if:
   *  - the reducer function is associative
   *  - we seed the computation with the identity of this function.
   */

  def foldMap2[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.map(_ + _)

  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def testFoldMap() = {
    assert(
      foldMap(Vector(1, 2, 3))(_.toString + "! ") == "1! 2! 3!"
    )

    assert(
      foldMap("Hello world!".toVector)(_.toString.toUpperCase) == "HELLO WORLD!"
    )
  }
}

