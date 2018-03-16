package scalanotes

/**
 * Ch.1 Anatomy of a Type Class
 */

// object Ch1TypeClasses {
object Main extends App {

  // 1.1.1 Type Classes

  // JSON Abstract Syntax Tree
  // sealed trait Json
  // final case class JsObject(get: Map[String, Json]) extends Json
  // final case class JsString(get: String) extends Json
  // final case class JsNumber(get: Double) extends Json
  // case object JsNull extends Json

  // Type class e.g., interface or API for some functionality.
  // This trait should encode JSON serialization.
  // trait JsonWriter[A] {
  //   def write(value: A): Json
  // }

  // 1.1.2 Type Class Instances
  //  - create concrete implementations
  //  - tag with `implicit`

  // final case class Person(name: String, email: String)

  // object JsonWriterInstances {
  //   implicit val stringWriter: JsonWriter[String] =
  //     new JsonWriter[String] {
  //       def write(value: String): Json = JsString(value)
  //     }

  //   implicit val personWriter: JsonWriter[Person] =
  //     new JsonWriter[Person] {
  //       def write(value: Person): Json =
  //         JsonObject(
  //           Map(
  //             "name" -> JsString(value.name),
  //             "email" -> JsString(value.email)
  //           )
  //         )
  //     }
  // }

  // 1.1.3 Type Class Interfaces
  //  - Interface:
  //    + functionality exposed to users
  //    + generic methods that take in instances of the type class as implicit parameters
  //    + Interface objects and syntax

  // Interface Objects
  //  - place methods in a singleton object

  // import JsonWriterInstances._

  // object Json {
  //   def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
  //     w.write(value)
  // }

  // Compiler will find implicit param JsonWriter from JsonWriterInstances
  //
  // scala> Json.toJson(Person("Dave", "dave@example.com"))
  //          -> Json.toJson(Person("Dave", "dave@example.com"))(personWriter)

  // The Implicitly Method
  //  - a generic type class interface
  //  - use implicitly to summon any value from the implicit scope by providing the type
  //  - good fallback for debugging:
  //    +  insert a call to implicitly within the general flow of our code to
  //       ensure the compiler can find an instance of a type class and ensure
  //       that there are no ambiguous implicit errors.

  // def implicitly[A](implicit value: A): A = value

  // implicitly[JsonWriter[String]]
  // res8: JsonWriter[String] = JsonWriterInstances$$anon$1@123ce4d6

  // 1.2 Working with Implicit values and parameters

  // 1.2.1 Packaging Implicits
  //  - definitions marked with implicit must be inside an object or trait
  //  - companion objects also work and will release implicits inside the
  //    companion class via implicit scope

  // 1.2.2 Implicit Scope
  //  - compiler searches for candidates in implicit scope at call site:
  //    + local or inherited definitions
  //    + imported defintions
  //    + definitions in companion object of type class
  //  - definitions are only included in implicit scope if tagged
  //  - errors with ambiguous implicit values for dups
  //  - implicit placements:
  //    + object: access by `import`
  //    + trait: access by inheritance
  //    + companion object of type class: always in scope
  //    + companion object of parameter type: always in scope

  // 1.2.3 Recursive Implicit Resolution
  //  - implicits are powerful in cases where the compiler will combine implict defintions during its serach
  //  - define implicit instances with:
  //    + implicit vals or objects
  //    + prepending `implicit` to functions

  // Generalizing implicit instances
  // Example: construct a JsonWriter for Option[A] using an implicit parameter to fill in A's
  // functionality.
  //
  // scala> Json.toJson(Option("A string"))
  //  -> Json.toJson(Option("A string"))(optionWriter(stringWriter))
  // implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
  //   new JsonWriter[Option[A]] {
  //     def write(option: Option[A]): Json =
  //       option match {
  //         case Some(aValue) => writer.write(aValue)
  //         case None => JsNull
  //       }
  //   }
  //
  // compare to:
  //
  // implicit val optionIntWriter: JsonWriter[Option[Int]] = ???
  // implicit val optionStringWriter: JsonWriter[Option[String]] = ???

  // 1.3 Printable Library Exercise

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val stringPrinter: Printable[String] =
      new Printable[String] {
        def format(a: String): String = a
      }
    implicit val integerPrinter: Printable[Int] =
      new Printable[Int] {
        def format(a: Int): String = a.toString
      }
  }

  object Printable {
    import PrintableInstances._

    def format(a: A)(implicit p: Printable[A]): String =
      a match {
        case Int    => p.format(a)
        case String => p.format(a)
      }

    def print[A](a: A)(implicit p: Printable[A]): Unit = println(p.format(a))
  }

  // Ch1 tests
  def tests() = {
  }
}

/**
 * Ch.8 Case Study: Testing Asynchronous Code
 */

import scala.language.higherKinds

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
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats.{ Monoid }
import cats.instances.int._ // for Monoid
import cats.instances.string._ // for Monoid
import cats.instances.future._ // for Monad and Monoid
import cats.syntax.semigroup._ // for |+|

object Ch9MapReduce {
  /**
   * Single-threaded map-reduce function.
   * Maps `f` over `sequence` and reduces using a `Monoid[B]`.
   *
   * Parallel fold will yield the correct results if:
   *  - the reducer function is associative
   *  - we seed the computation with the identity of this function.
   */

  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| f(_))

  // Use the algorithum and techniques described in Ch9 to:
  //  - Split the work into batches, one batch per CPU.
  //  - Process each batch in a parallel thread.
  //  - Refer back to Figure 15 if you need to review the overall algorithm.
  //  - process the batches for each CPU using your implementation of foldMap
  //
  // In more detail:
  // - we start with an initial list of all the data we need to process;
  // - we divide the data into batches, sending one batch to each CPU;
  // - the CPUs run a batch-level map phase in parallel;
  // - the CPUs run a batch-level reduce phase in parallel, producing a local result for each batch;
  // - we reduce the results for each batch to a single final result.
  //
  // Techniques used:
  //   ...

  val CPU_NUM = Runtime.getRuntime.availableProcessors

  def parallelFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = ???

  // Ch9 tests
  def testFoldMap() = {
    assert(foldMap(Vector(1, 2, 3))(_.toString + "! ") == "1! 2! 3!")
    assert(foldMap("Hello world!".toVector)(_.toString.toUpperCase) == "HELLO WORLD!")
  }
}

