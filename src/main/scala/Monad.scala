
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}

object Monads {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] = parseInt(aStr).flatMap { aNum =>
    parseInt(bStr).flatMap { bNum =>
      divide(aNum, bNum)
    }
  }

  def stringDivideByForExpr(aStr: String, bStr: String): Option[Int] = for {
    aNum <- parseInt(aStr)
    bNum <- parseInt(bStr)
    ans <- divide(aNum, bNum)
  } yield ans

  def main(args: Array[String]): Unit = {
    // Monadic Laws
    println(Option(10).flatMap(x => Option(x.toDouble)) == Option[Double](10))
    println(Option(10).flatMap(Option(_)) == Option(10))
    println(Option(10).flatMap(x => Option(x.toDouble)).flatMap(x => Option(x.toString)) ==
      Option(10).flatMap(x => Option(x.toDouble).flatMap(y => Option(y.toString))))

    println(stringDivideBy("6", "2"))
    println(stringDivideBy("6", "0"))
    println(stringDivideBy("6", "foo"))
    println(stringDivideBy("bar", "2"))

    println(stringDivideByForExpr("6", "2"))
    println(stringDivideByForExpr("6", "0"))
    println(stringDivideByForExpr("6", "foo"))
    println(stringDivideByForExpr("bar", "2"))

    // Exercise: Ge􏰅ng Func-y
    val monad = new Monad[Option] {
      override def pure[A](a: A): Option[A] = Option(a)

      override def flatMap[A, B](value: Option[A])(func: A => Option[B]): Option[B] = func(value.get)
    }

    println(monad.map(Option(900))(_.toDouble))
  }

  // Exercise: Ge􏰅ng Func-y
  // Every monad is also a functor. We can define map in the same way for every monad using the existing methods, flatMap and pure:
  import scala.language.higherKinds

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)((a: A) => pure(func(a)))
  }

}

object CatsMonads {

  def main(args: Array[String]): Unit = {
    monadExamples
    monadFuture
    monadSyntax
    genericMonad
  }

  def monadExamples = {
    import cats.Monad
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad

    val opt1 = Monad[Option].pure(3)
    println(opt1)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    println(opt2)
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    println(opt3)

    val list1 = Monad[List].pure(3)
    val list2 = Monad[List].
      flatMap(List(1, 2, 3))(a => List(a, a * 10))
    val list3 = Monad[List].map(list2)(a => a + 123)

    println(list1)
    println(list2)
    println(list3)

    import cats.instances.vector._ // for Monad
    println(Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10)))
  }

  def monadFuture = {
    //Cats also provides a Monad for Future. Unlike the methods on the Future class itself, the pure and flatMap methods
    // on the monad can’t accept implicit ExecutionContext parameters (because the parameters aren’t part of the definitions
    // in the Monad trait). To work around this, Cats requires us to have an ExecutionContext in scope when we summon a Monad for Future
    import cats.Monad
    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.future._
    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

    import scala.concurrent.duration._
    println(Await.result(future, 1.second))
  }

  def monadSyntax = {
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad
    import cats.syntax.applicative._ // for pure
    println(1.pure[Option])
    println(1.pure[List])
  }

  def genericMonad = {
    import cats.Monad
    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap
    import scala.language.higherKinds
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad

    //It’s difficult to demonstrate the flatMap and map methods directly on Scala monads like Option and List,
    // because they define their own explicit versions of those methods. Instead we’ll write a generic function that
    // performs a calculation on parameters that come wrapped in a monad of the user’s choice:
    def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x * x + y * y))

    def sumSquareWithForComprehensions[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = for {
      x <- a
      y <- b
    } yield x * x + y * y

    println(sumSquare(Option(3), Option(4)))
    println(sumSquare(List(1, 2, 3), List(4, 5)))

    println(sumSquareWithForComprehensions(Option(3), Option(4)))
    println(sumSquareWithForComprehensions(List(1, 2, 3), List(4, 5)))
    println(List(1, 2, 3).flatMap(x => (List(9, 8)).flatMap(y => List(3, 4, 5).map(z => x * x + y * y + z))))
  }
}

object CatsIdentityMonad {

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._
  import scala.language.higherKinds

  //  package cats
  //  type Id[A] = A
  //  Id is actually a type alias that turns an atomic type into a single-parameter type constructor.
  //    We can cast any value of any type to a corresponding Id
  def main(args: Array[String]): Unit = {
    import cats.Id
    val d: Id[Int] = 12
    println(sumSquare(3: Id[Int], 4: Id[Int]))
    println("Dave": Id[String])
    println(List(1, 2, 3): Id[List[Int]])

    val a = Monad[Id].pure(3)
    val b = Monad[Id].flatMap(a)(_ + 1)
    println(a)
    println(b)
    val c = for {
      x <- a
      y <- b
    } yield x + y
    println(c)
  }

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x * x + y * y

}

object IdentityMonad {
  type Id[A] = A


  object IdMonadC {

    // The final punch line is that, once we strip away the Id type constructors, flatMap and map are actually identical:
    //This ties in with our understanding of functors and monads as sequencing type classes. Each type class allows us
    // to sequence operations ignoring some kind of complication. In the case of Id there is no complication,
    // making map and flatMap the same thing.
    //  Notice that we haven’t had to write type annotations in the method bodies above. The compiler is able to interpret
    // values of type A as Id[A] and vice versa by the context in which they are used.
    //The only restriction we’ve seen to this is that Scala cannot unify types and type constructors when searching for
    // implicits. Hence our need to re-type Int as Id[Int] in the call to sumSquare at the opening of this section:
    class IdMonad[A] {
      def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

      def pure[A](x: A): Id[A] = x

      def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
    }

    implicit val intIdMonad = new IdMonad[Int]

    def apply[A](implicit m: IdMonad[A]): IdMonad[A] = m

    //    def pure[A](id: Id[A])(implicit m: IdMonad[A]) = m.pure(id)
    //    def flatMap[A,B](fa: Id[A])(f: A => Id[B])(implicit m: IdMonad[A]): Id[B] = m.flatMap(fa)(f)
    //    def map[A, B](fa: Id[A])(f: A => B)(implicit m: IdMonad[A]): Id[B] = m.map(fa)(f)

    implicit class IdMonadDecorator[A](value: A) {
      def pure(implicit m: IdMonad[A]) = m.pure(value)

      def flatMap[B](f: A => Id[B])(implicit m: IdMonad[A]): Id[B] = m.flatMap[A, B](value)(f)

      def map[B](f: A => B)(implicit m: IdMonad[A]): Id[B] = m.map[A, B](value)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    import IdMonadC._

    println(12: Id[Int])
    println(IdMonadC[Int].pure(12))
    println(IdMonadC[Int].flatMap(12)(_ * 90))
    println(IdMonadC[Int].map(12)(_.toDouble))
    val x: Id[Int] = 10
    println(10.pure)
    println(x.flatMap(i => i))
    val y: Id[Int] = 90
    println(y.map(_.toDouble))
    println(for {
      a <- x
      b <- y
    }
      yield a * b)
  }
}

object CatsEitherMonad {
  def main(args: Array[String]): Unit = {
    println(creatingEither)
    println(creatingInstancesUsingSmartConstructors)
    println(countPositive(List(1, 2, 3)))
    println(countPositive(List(1, -2, 3)))

    import cats.syntax.either._
    println(Either.catchOnly[NumberFormatException]("foo".toInt))
    println(Either.catchNonFatal(sys.error("Badness")))

    // There are also methods for creating an Either from other data types:
    println(Either.fromTry(scala.util.Try("foo".toInt)))
    println(Either.fromOption[String, Int](None, "Badness"))

    //cats.syntax.either also adds some useful methods for instances of Either. We can use orElse and getOrElse to
    // extract values from the right side or return a default:
    println("Error".asLeft[Int].getOrElse(0))
    println("Error".asLeft[Int].orElse(2.asRight[String]))
    println(Either.fromTry(scala.util.Try("90".toInt)).orElse(33.asRight[Int]))

    // The ensure method allows us to check whether the right-hand value satisfies a predicate:
    println(-1.asRight[String].ensure("Must be non-negative!")(_ > 0))

    // The recover and recoverWith methods provide similar error handling to their namesakes on Future:
    println(Either.fromTry(scala.util.Try("90r".toInt)).recover {
      case _: Throwable => -100
    })
    println("error".asLeft[Int].recoverWith {
      case str: String => Right(-1)
    })

    // There are leftMap and bimap methods to complement map:
    println("foo".asLeft[Int].leftMap(_.reverse))
    println(6.asRight[String].bimap(_.reverse, _ * 7))
    println("bar".asLeft[Int].bimap(_.reverse, _ * 7))

    // The swap method lets us exchange le􏰃 for right:
    println(123.asRight[String])
    // res19: Either[String,Int] = Right(123)
    println(123.asRight[String].swap)
    // res20: scala.util.Either[Int,String] = Left(123)

    println(123.asRight[String].toOption)
    println(123.asRight[String].swap.toOption)
  }

  def creatingEither = {
    val either1: Either[String, Int] = Right(10)
    val either2: Either[String, Int] = Right(32)
    for {
      a <- either1
      b <- either2
    } yield a + b
  }

  def creatingInstancesUsingSmartConstructors = {
    import cats.syntax.either._ // for asRight
    val a = 3.asRight[String]
    val b = 4.asRight[String]
    println(a.getClass)
    println(b)
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  }

  // These “smart constructors” have advantages over Left.apply and Right.apply because they return results of type
  // Either instead of Left and Right. This helps avoid type inference bugs caused by over-narrowing, like the bug
  // in the example below:
  //  def countPositive(nums: List[Int]) =
  //    nums.foldLeft(Right(0)) { (accumulator, num) =>
  //      if (num > 0) {
  //        accumulator.map(_ + 1)
  //      } else {
  //        Left("Negative. Stopping!")
  //      }
  //    }

  import cats.syntax.either._

  def countPositive(nums: List[Int]) = nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
    if (num > 0) {
      accumulator.map(_ + 1)
    } else {
      Left("Negative. Stopping!")
    }
  }
}

object CatsEitherErrorHandling {
  // Error Handling
  //Either is typically used to implement fail-fast error handling. We sequence computations using flatMap as usual.
  // If one computa􏰀on fails, the remaining computations are not run:
  def handleError = {
    import cats.syntax.either._
    for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String]
      d <- 9.asRight[Int]
    } yield c * 100
  }

  // When using Either for error handling, we need to determine what type we want to use to represent errors.
  // We could use Throwable for this:
  //  type Result[A] = Either[Throwable, A]
  //This gives us similar semantics to scala.util.Try. The problem, however, is that Throwable is an extremely
  // broad type. We have (almost) no idea about what type of error occurred.
  //Another approach is to define an algebraic data type to represent errors that may occur in our program:
  sealed trait LoginError extends Product with Serializable

  final case class UserNotFound(username: String)
    extends LoginError

  final case class PasswordIncorrect(username: String) extends LoginError

  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  //This approach solves the problems we saw with Throwable. It gives us a fixed set of expected error types and a
  // catch-all for anything else that we didn’t expect. We also get the safety of exhaustivity checking on any pattern
  // matching we do:
  // Choose error-handling behaviour based on type:
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")
      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")
      case UnexpectedError =>
        println(s"Unexpected error")
    }


  //  Here is a simplified version of the definition of MonadError:
  //  package cats
  //  trait MonadError[F[_], E] extends Monad[F] {
  //    // Lift an error into the `F` context:
  //    def raiseError[A](e: E): F[A]
  //    // Handle an error, potentially recovering from it:
  //    def handleError[A](fa: F[A])(f: E => A): F[A]
  //    // Test an instance of `F`,
  //    // failing if the predicate is not satisfied:
  //    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  //  }

  def monadError = {

    import cats.MonadError
    import cats.instances.either._ // for MonadError
    type ErrorOr[A] = Either[String, A]
    val monadError = MonadError[ErrorOr, String]
    val success: ErrorOr[Int] = monadError.pure(42)
    // success: ErrorOr[Int] = Right(42)
    val failure: ErrorOr[Nothing] = monadError.raiseError("Badness1")
    println("Failure --> " + failure)
    // failure: ErrorOr[Nothing] = Left(Badness)

    //handleError is the complement of raiseError. It allows us to consume an error and (possibly) turn it into a success,
    // similar to the recover method of Future:
    val result = monadError.handleError(failure) {
      case "Badness" =>
        monadError.pure("It's ok")
      case other =>
        monadError.raiseError("It's not ok")
    }
    println(result)

    val result1 = monadError.handleError(monadError.raiseError("Badness")) {
      case "Badness" =>
        "It's ok"
      case other =>
        "It's not ok"
    }
    println("Simple Error Handle --> " + result1)

    println(monadError.ensure(success)("Number too low!")(_ > 1000))
    // res3: ErrorOr[Int] = Left(Number too low!)

    import cats.syntax.applicative._ // for pure
    import cats.syntax.applicativeError._ // for raiseError etc
    import cats.syntax.monadError._ // for ensure
    val success1 = 42.pure[ErrorOr]
    // success: ErrorOr[Int] = Right(42)
    val failure1 = "Badness".raiseError[ErrorOr, Int]
    // failure: ErrorOr[Int] = Left(Badness)

    println(success1.ensure("Number low!")(_ > 1000))
    // res4: Either[String,Int] = Left(Number to low!)

    // Cats provides instances of MonadError for numerous data types including Either, Future, and Try. The instance
    // for Either is customisable to any error type, whereas the instances for Future and Try always represent
    // errors as Throwables
  }

  def main(args: Array[String]): Unit = {
    println(handleError)

    import cats.syntax.either._
    val result1: LoginResult = User("dave", "passw0rd").asRight
    // result1: LoginResult = Right(User(dave,passw0rd))
    val result2: LoginResult = UserNotFound("dave").asLeft
    // result2: LoginResult = Left(UserNotFound(dave))

    result1.fold(handleError, println)
    // User(dave,passw0rd)
    result2.fold(handleError, println)
    // User not found: dave

    monadError
  }
}

object CatsEvalMonad {

  import cats.Eval
  // Eval computation model
  // Now = val, Later = lazy val, Always = def
  // val is eager memoized, def is lazy not memoized, lazy val is lazy memoized

  def randomNumbers = {
    val now = Eval.now(math.random + 1000)
    println(now.value)
    println(now.value)

    val later = Eval.later(math.random + 2000)
    println(later.value)
    println(later.value)

    val always = Eval.always(math.random + 3000)
    println(always.value)
    println(always.value)
  }

  def evalAsMonad = {
    val greeting = Eval.always {
      println("Step 1");
      "Hello"
    }.
      map { str => println("Step 2"); s"$str world" }
    println(greeting.value)

    val ans = for {
      a <- Eval.now {
        println("Calculating A");
        40
      }
      b <- Eval.always {
        println("Calculating B");
        2
      }
    } yield {
      println("Adding A and B")
      a + b
    }
    println(ans.value)
    println(ans.value)


    val saying = Eval.
      always {
        println("Step 1");
        "The cat"
      }.
      map { str => println("Step 2"); s"$str sat on" }.
      memoize.
      map { str => println("Step 3"); s"$str the mat" }
    println(saying.value)
    println(saying.value)
  }

  def evalTrampolining = {
    // One useful property of Eval is that its map and flatMap methods are trampolined. This means we can nest calls to
    // map and flatMap arbitrarily without consuming stack frames. We call this property “stack safety”.
    def factorial(n: BigInt): BigInt =
      if (n == 1) n else n * factorial(n - 1)
    //It is relatively easy to make this method stack overflow:
    //factorial(50000)

    // The defer method is tram- polined like map and flatMap, so we can use it as a quick way to make an existing
    // operation stack safe:
    def factorialWithEvalTrampolining(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorialWithEvalTrampolining(n - 1).map(_ * n))
      }

    println(factorialWithEvalTrampolining(50000).value)
    var sum: Int = 0
    println(foldRight(1 to 50000 toList, sum)((x, y) => x + y).value)
  }

  def main(args: Array[String]): Unit = {
    randomNumbers
    evalAsMonad
    evalTrampolining
  }

  // Exercise: Safer Folding using Eval
  // The naive implementation of foldRight below is not stack safe. Make it so using Eval:
  //DONE
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
    case head :: tail =>
      Eval.defer(foldRight(tail, acc)(fn).map(x => fn(head, x)))
    case Nil =>
      Eval.now(acc)
  }


  // Eval is a useful tool to enforce stack safety when working on very large computations and data structures. However,
  // we must bear in mind that trampolining is not free. It avoids consuming stack by creating a chain of function
  // objects on the heap. There are still limits on how deeply we can nest computations, but they are bounded by the
  // size of the heap rather than the stack.
}

object CatsWriterMonad {
  // cats.data.Writer is a monad that lets us carry a log along with a computation. We can use it to record messages,
  // errors, or additional data about a computation, and extract the log alongside the final result.

  // One common use for Writers is recording sequences of steps in multithreaded computations where standard imperative
  // logging techniques can result in interleaved messages from different contexts. With Writer the log for the
  // computation is tied to the result, so we can run concurrent computations without mixing logs.

  // Cats Data Types
  // Writer is the first data type we’ve seen from the cats.data package. This package provides instances of various
  // type classes that produce useful semantics. Other examples from cats.data include the monad transformers that we
  // will see in the next chapter, and the Validated type we will encounter in Chapter 6.

  // A Writer[W, A] carries two values: a log of type W and a result of type A. We can create a Writer from values of
  // each type as follows:
  def writerMonad = {
    import cats.data.Writer
    import cats.instances.vector._ // for Monoid
    val w = Writer(Vector(
      "It was the best of times",
      "it was the worst of times"
    ), 1859)

    println(w)

    // Writer is a type alias for WriterT, so we can read types like WriterT[Id, W, A] as Writer[W, A]
    // type Writer[W, A] = WriterT[Id, W, A]

    //import cats.instances.vector._   // for Monoid
    import cats.syntax.applicative._ // for pure
    type Logged[A] = Writer[Vector[String], A]
    println(123.pure[Logged])

    // If we have a log and no result we can create a Writer[Unit] using the tell syntax from cats.syntax.writer:
    import cats.syntax.writer._ // for tell
    println(Vector("msg1", "msg2", "msg3").tell)

    // If we have both a result and a log, we can either use Writer.apply or we can use the writer syntax from
    // cats.syntax.writer:
    import cats.syntax.writer._ // for writer
    val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
    val b = 123.writer(Vector("msg1", "msg2", "msg3"))
    println(a)
    println(b)

    val aResult: Int = a.value
    val aLog: Vector[String] = a.written
    println(s"Result --> $aResult")
    println(s"Logs --> $aLog")

    val (log, result) = b.run
    println(s"Result --> $result")
    println(s"Logs --> $log")
  }

  def composingAndTransformingWriters = {
    import cats.syntax.applicative._ // for pure
    import cats.syntax.writer._ // for writer
    import cats.data.Writer
    import cats.instances.vector._ // for Monoid

    type Logged[A] = Writer[Vector[String], A]

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    // translate for expression to fluent api calls
    // println(10.pure[Logged].flatMap(x => Vector("a", "b", "c").tell.flatMap(y => 32.writer(Vector("x", "y", "z")).map(z => x + z))))
    val (log, result) = writer1.run

    println(s"composingAndTransformingWriters' Result --> $result")
    println(s"composingAndTransformingWriters' Logs --> $log")

    // In addition to transforming the result with map and flatMap, we can transform the log in a Writer with the mapWritten method:
    val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
    println("MapWritten -> " + writer2)

    // We can transform both log and result simultaneously using bimap or mapBoth. bimap takes two function parameters,
    // one for the log and one for the result. mapBoth takes a single func􏰀on that accepts two parameters:
    val writer3 = writer1.bimap(
      log => log.map(_.toUpperCase),
      res => res * 100
    )
    println("Bimap1 -> " + writer3.run)

    val writer4 = writer1.mapBoth { (log, res) =>
      val log2 = log.map(_ + "!")
      val res2 = res * 1000
      (log2, res2)
    }
    println("Bimap2 -> " + writer4.run)

    // Finally, we can clear the log with the reset method and swap log and result with the swap method:
    val writer5 = writer1.reset
    println("Reset -> " + writer5.run)
    val writer6 = writer1.swap
    println("Swap -> " + writer6.run)
  }

  def excerciseOnFactorial = {
    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    import cats.data.Writer
    import cats.syntax.writer._ // for writer
    import cats.syntax.applicative._
    import cats.instances.vector._
    type Logged[A] = Writer[Vector[String], A]

    def factorialWithForExpr(n: Int): Logged[Int] =
      for {
        ans <- if (n == 0) {
          1.pure[Logged]
        } else {
          slowly(factorialWithForExpr(n - 1).map(_ * n))
        }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    //val x = 1.pure[Logged].flatMap(ans => Vector(s"fact 10 $ans").tell.map(_ => ans))

    def factorial(w: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      slowly({
        w.mapBoth { (log, res) =>
          if (res == 0)
            (Vector(s"fact $res 1"), 1)
          else {
            (factorial((res - 1).writer(Vector.empty[String])).mapBoth {
              (ilog, ires) =>
                val ans = res * ires
                (ilog.map(_ + s", fact $res $ans"), res * ires)
            }).run
          }
        }
      })
    }

    def fact(n: Int) = {
      println(s"factorial($n) = ${factorialWithForExpr(n)}")
    }

    println(factorial(5.writer(Vector.empty[String])).run)

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    Await.result(Future.sequence(Vector(
      Future(fact(3)),
      Future(fact(6))
    )), 5.seconds)

  }

  def main(args: Array[String]): Unit = {
    writerMonad
    composingAndTransformingWriters
    excerciseOnFactorial
  }
}

object CatsReaderMonad {
  def creatingAndUnpackingReaders = {
    import cats.data.Reader
    case class Cat(name: String, favoriteFood: String)
    val catName: Reader[Cat, String] =
      Reader(cat => cat.name)

    println(catName.run(Cat("Garfield", "lasagne")))

    val greetKitty: Reader[Cat, String] =
      catName.map(name => s"Hello ${name}")

    println(greetKitty.run(Cat("Heathcliff", "junk food")))

    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed <- feedKitty
      } yield s"$greet. $feed."

    println(greetAndFeed(Cat("Garfield", "lasagne")))

    println(greetAndFeed(Cat("Heathcliff", "junk food")))
  }

  def main(args: Array[String]): Unit = {
    creatingAndUnpackingReaders
  }
}

object HackingOnReaders {

  import cats.data.Reader

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).getOrElse("") == password)

  def checkLogin(
                  userId: Int,
                  password: String): DbReader[Boolean] =
  //findUsername(userId).flatMap(user => checkPassword(user.getOrElse(""), password))
    for {
      user <- findUsername(userId)
      result <- checkPassword(user.getOrElse(""), password)
    }
      yield result

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)

    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(4, "davinci").run(db))
  }
}

object CatsStateMonad {
  // an instance of State is a function does two things:
  // • transformsaninputstatetoanoutputstate;
  // • computes a result.

  // We can “run” our monad by supplying an initial state. State provides three methods—run, runS, and runA—that return
  // different combinations of state and result. Each method returns an instance of Eval, which State uses to
  // maintain stack safety
  def creatingAndUnpackingState = {
    import cats.data.State
    val a = State[Int, String] { state =>
      (state, s"The state is $state")
    }

    val (state, result) = a.run(10).value
    val state2 = a.runS(10).value
    val result2 = a.runA(10).value

    println(s"Result1 = $result, State1 = $state")
    println(s"Result2 = $result2, State2 = $state2")
  }

  // As we’ve seen with Reader and Writer, the power of the State monad comes from combining instances.
  // The map and flatMap methods thread the state from one instance to another. Each individual instance represents an
  // atomic state transformation, and their combination represents a complete sequence of changes:
  def composingAndTransformingState = {
    import cats.data.State
    val step1 = State[Int, String] { num =>
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }

    val step2 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)
    val (state, result) = both.run(20).value
    println(s"$result ..($state)")

    // The general model for using the State monad is to represent each step of a computation as an instance and
    // compose the steps using the standard monad operators. Cats provides several convenience constructors for
    // creating primitive steps:
    //  • get extracts the state as the result;
    //  • set updates the state and returns unit as the result;
    //  • pure ignores the state and returns a supplied result;
    //  • inspect extracts the state via a transformation function; (Inspect a value from the input state, without modifying the state.)
    //  • modify updates the state using an update function.

    val getDemo = State.get[Int]
    println("Get -> " + getDemo.run(10).value)

    val setDemo = State.set[Int](30)
    println("Set -> " + setDemo.run(10).value)

    val pureDemo = State.pure[Int, String]("Pure result")
    println("Pure -> " + pureDemo.run(10).value)

    val inspectDemo = State.inspect[Int, String](_ + "!")
    println("Inspect -> " + inspectDemo.run(10).value)

    val modifyDemo = State.modify[Int](_ + 1)
    println("Modify -> " + modifyDemo.run(10).value)

    import State._
    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)

    //    val (s, r) = get[Int].flatMap(x => set[Int](x + 1).flatMap(y => get[Int]
    //      .flatMap(z => modify[Int](_ + 1).map(h => inspect[Int,Int](_ * 80)).map(l => (x, z, l.map(m => m)))))).run(90).value
    //    println("Manual Calls => " + r)

    val (stateF, resultF) = program.run(1).value
    println(s"For Expr Result = $resultF, State = $stateF")

  }

  def PostOrderCalculator = {
    import cats.data.State
    type CalcState[A] = State[List[Int], A]

    def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
      val newStack = sym match {
        case "/" => (oldStack.tail.head / oldStack.head) :: oldStack.tail.tail
        case "*" => (oldStack.tail.head * oldStack.head) :: oldStack.tail.tail
        case "+" => (oldStack.tail.head + oldStack.head) :: oldStack.tail.tail
        case "-" => (oldStack.tail.head - oldStack.head) :: oldStack.tail.tail
        case _ => sym.toInt :: oldStack
      }
      (newStack, newStack.head)
    }

    import cats.syntax.applicative._ // for pure
    def evalAll(input: List[String]): CalcState[Int] =
    // This Works but foldleft is better
    //      input match {
    //        case sym :: restOfInput => for {
    //          _ <- evalOne(sym)
    //          ans <- evalAll(restOfInput)
    //        }
    //          yield ans
    //
    //        case Nil => State[List[Int], Int]{
    //          st => (st, st.head)
    //        }
    //      }
      input.foldLeft(0.pure[CalcState]) { (a, b) =>
        a.flatMap(_ => evalOne(b))
      }


    println("Starting PostOrder.....")

    println(evalOne("42").runA(Nil).value)

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    println(program.runA(Nil).value)

    val program2 = evalAll(List("1", "2", "+", "3", "*"))
    // program: CalcState[Int] = cats.data.IndexedStateT@f0a1bee
    println(program2.runA(Nil).value)

    val program3 = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    println(program3.runA(Nil).value)
  }

  def main(args: Array[String]): Unit = {
    creatingAndUnpackingState
    composingAndTransformingState
    PostOrderCalculator
  }
}

object CustomMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  import cats.Monad

  implicit val treeMonad = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    def tailRecM_NoTailRecursion[A, B](a: A)
                                      (func: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(func(a)) {
        case Left(value) =>
          tailRecM(value)(func)
        case Right(value) =>
          Leaf(value)
      }

    def tailRecM[A, B](arg: A)
                      (func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
                open: List[Tree[Either[A, B]]],
                closed: List[Option[Tree[B]]]): List[Tree[B]] = open match {
        case Branch(l, r) :: next =>
          loop(l :: r :: next, None :: closed)
        case Leaf(Left(value)) :: next =>
          loop(func(value) :: next, closed)
        case Leaf(Right(value)) :: next =>
          loop(next, Some(pure(value)) :: closed)
        case Nil =>
          closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
            maybeTree.map(_ :: acc).getOrElse {
              val left :: right :: tail = acc
              branch(left, right) :: tail
            }
          }
      }

      loop(List(func(arg)), Nil).head
    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }

  def main(args: Array[String]): Unit = {
    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap
    println(branch(leaf(100), leaf(200)).
      flatMap(x => branch(leaf(x - 1), leaf(x + 1))))

    val t = for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c
    println(t)
  }

}