package CaseStudies

// Type classes like Functor, Applicative, Monad, and Traverse provide abstract implementations of patterns such as
// mapping, zipping, sequencing, and itera􏰀on. The mathematical laws on those types ensure that they work together with
// a consistent set of semantics.

object TestingAsynchronousCode {

  def main(args: Array[String]): Unit = {
    testTotalUptime
  }

  import scala.concurrent.Future

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  import cats.instances.future._ // for Applicative
  import cats.Applicative._
  import cats.syntax.applicative._
  import cats.instances.list._ // for Traverse
  import cats.syntax.traverse._ // for traverse
  import scala.concurrent.ExecutionContext.Implicits.global

  type Id[A] = A

  // Doesn't work
  //  class UptimeService[F[_]](client: UptimeClient[F[Int]]) {
  //    def getTotalUptime(hostnames: List[String]): F[Int] =
  //      hostnames.traverse(client.getUptime).map(_.sum)
  //  }
  // The problem here is that traverse only works on sequences of values that have an Applicative. In our original code
  // we were traversing a List[Future[Int]]. There is an applicative for Future so that was fine. In this version we
  // are traversing a List[F[Int]]. We need to prove to the compiler that F has an Applicative.
  // Do this by adding an implicit constructor parameter to UptimeService.

  import cats.Applicative

  // Functor syntax required for map on traverse
  import cats.syntax.functor._

  class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }

  class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future] {
    override def getUptime(hostname: String): Future[Int] = Future(hosts.getOrElse(hostname, 0))
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val testClient = new TestUptimeClient(hosts)

    val expected = hosts.values.sum
    //println(actual.value.get)
    //println(expected)

    assert(actual == expected)
  }

}
