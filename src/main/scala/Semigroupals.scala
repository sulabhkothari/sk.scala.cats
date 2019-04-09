object Semigroupals {
  def main(args: Array[String]): Unit = {
    import cats.Semigroupal
    import cats.instances.option._ // for Semigroupal
    println(Semigroupal[Option].product(Some(123), Some("abc")))
    println(Semigroupal[Option].product(None, Some("abc")))
    println(Semigroupal[Option].product(Some(123), None))

    joiningMultipleArities
    applySyntax
    semigroupalAppliedToDifferentType
    excerciseTheProductOfMonads
    validated
    excerciseFormValidation
  }

  def joiningMultipleArities = {
    import cats.Semigroupal
    import cats.instances.option._ // for Semigroupal Semigroupal.tuple3(Option(1), Option(2), Option(3))
    println("Semigroupal1 on Options --> " + Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]))

    println("Semigroupal2 on Options --> " + Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))
    println("Semigroupal3 on Options --> " + Semigroupal.map2(Option(1), Option.empty[Int])(_ + _))

    // There are also methods contramap2 through contramap22 and imap2 through imap22, that require instances of
    // Contravariant and Invariant respectively.
  }

  def applySyntax = {
    // Cats provides a convenient apply syntax that provides a shorthand for the methods described above.
    import cats.instances.option._ // for Semigroupal
    import cats.syntax.apply._ // for tupled and mapN

    // The tupled method is implicitly added to the tuple of Options. It uses the Semigroupal for Option to zip the
    // values inside the Options, creating a single Option of a tuple:
    println("Tupled1 --> " + (Option(123), Option("abc")).tupled)

    // We can use the same trick on tuples of up to 22 values. Cats defines a separate tupled method for each arity
    println("Tupled1 --> " + (Option(123), Option("abc"), Option(true)).tupled)

    // In addition to tupled, Cats’ apply syntax provides a method called mapN that accepts an implicit Functor and a
    // function of the correct arity to combine the values:
    case class Cat(name: String, born: Int, color: String)

    println("MapN with implicit Functor --> " + (
      Option("Garfield"),
      Option(1978),
      Option("Orange & black")
    ).mapN(Cat.apply))

    // Internally mapN uses the Semigroupal to extract the values from the Option and the Functor to apply the values
    // to the function. It’s nice to see that this syntax is type checked. If we supply a function that accepts the
    // wrong number or types of parameters, we get a compile error:

    // val add: (Int, Int) => Int = (a, b) => a + b
    // add: (Int, Int) => Int = <function2>
    // (Option(1), Option(2), Option(3)).mapN(add)
    // <console>:27: error: type mismatch;
    // found : (Int, Int) => Int
    // required: (Int, Int, Int) => ?
    // (Option(1), Option(2), Option(3)).mapN(add)
    //
    // (Option("cats"), Option(true)).mapN(add)
    // <console>:27: error: type mismatch
    // found : (Int, Int) => Int
    // required: (String, Boolean) => ?
    // (Option("cats"), Option(true)).mapN(add)
    //                                     ^
  }

  def fancyFunctorsAndApplySyntax = {
    // Apply syntax also has contramapN and imapN methods that accept Contravariant and Invariant functors.
    // For example, we can combine Monoids using In- variant. Here’s an example:

    import cats.Monoid
    import cats.instances.int._
    import cats.instances.invariant._
    import cats.instances.list._
    import cats.instances.string._
    import cats.syntax.apply._
    case class Cat(
                    name: String,
                    yearOfBirth: Int,
                    favoriteFoods: List[String]
                  )

    val tupleToCat: (String, Int, List[String]) => Cat =
      Cat.apply _
    val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

    import cats.syntax.semigroup._ // for |+|
    val garfield = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
    garfield |+| heathcliff
  }

  def semigroupalAppliedToDifferentType = {
    // Semigroupal doesn’t always provide the behaviour we expect, particularly
    //for types that also have instances of Monad. We have seen the behaviour of
    //the Semigroupal for Option. Let’s look at some examples for other types.
    def futuresApply = {
      import cats.Semigroupal
      import cats.instances.future._ // for Semigroupal
      import scala.concurrent._
      import scala.concurrent.duration._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.language.higherKinds
      val futurePair = Semigroupal[Future].
        product(Future("Hello"), Future(123))
      println("Futures1 --> " + Await.result(futurePair, 1.second))

      // The two Futures start executing the moment we create them, so they are
      //already calculating results by the time we call product. We can use apply
      //syntax to zip fixed numbers of Futures:

      import cats.syntax.apply._ // for mapN
      case class Cat(
                      name: String,
                      yearOfBirth: Int,
                      favoriteFoods: List[String]
                    )

      val futureCat = (
        Future("Garfield"),
        Future(1978),
        Future(List("Lasagne"))
      ).mapN(Cat.apply)
      println("Futures1 --> " + Await.result(futureCat, 1.second))
    }

    def listsApply = {
      // Combining Lists with Semigroupal produces some potentially unexpected
      //results. We might expect code like the following to zip the lists, but we actually
      //get the cartesian product of their elements:
      import cats.Semigroupal
      import cats.instances.list._ // for Semigroupal
      println("List --> " + Semigroupal[List].product(List(1, 2), List(3, 4)))
    }

    def eitherApply = {
      // We opened this chapter with a discussion of fail-fast versus accumulating
      //error-handling. We might expect product applied to Either to accumulate
      //errors instead of fail fast. Again, perhaps surprisingly, we find that product
      //implements the same fail-fast behaviour as flatMap:

      import cats.instances.either._ // for Semigroupal
      import cats.Semigroupal

      type ErrorOr[A] = Either[Vector[String], A]
      println("Either --> " + Semigroupal[ErrorOr].product(
        Left(Vector("Error 1")),
        Left(Vector("Error 2"))
      ))
    }

    futuresApply
    listsApply
    eitherApply

    // The reason for the surprising results for List and Either is that they are both monads. To ensure consistent
    // semantics, Cats’ Monad (which extends Semigroupal) provides a standard definition of product in terms of map and
    // flatMap. This gives what we might think of as unexpected and less useful behaviour for a number of data types.
    // The consistency of semantics is important for higher level abstractions, but we don’t know about those yet.

    // So why bother with Semigroupal at all? The answer is that we can create useful data types that have instances of
    // Semigroupal (and Applicative) but not Monad. This frees us to implement product in different ways. We’ll examine
    // this further in a moment when we look at an alternative data type for error handling.
  }

  def excerciseTheProductOfMonads = {
    import cats.Monad
    import cats.syntax.flatMap._ // for flatMap
    import cats.syntax.functor._
    import cats.instances.list._
    import cats.instances.either._
    def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
      //Monad[M].flatMap(x)(a => Monad[M].map(y)(b => (a,b)))
      for {
        a <- x
        b <- y
      } yield (a, b)
    }

    println("Semigroupal using Monad1 (Excercise) --> " + product(List(10, 12), List("sdf", "abc")))

    type ErrorOr[A] = Either[Vector[String], A]
    println("Semigroupal using Monad2 (Excercise) --> " + product[ErrorOr, Int, Int](
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    ))
  }

  def validated = {
    def createInstancesOfValidated = {
      // By now we are familiar with the fail-fast error handling behaviour of Either. Furthermore, because Either is a
      // monad, we know that the semantics of product are the same as those for flatMap. In fact, it is impossible for us
      // to design a monadic data type that implements error accumulating semantics without breaking the consistency of
      // these two methods.

      // Fortunately, Cats provides a data type called Validated that has an instance of Semigroupal but no instance of
      // Monad. The implementation of product is therefore free to accumulate errors:

      import cats.Semigroupal
      import cats.data.Validated
      import cats.instances.list._ // for Monoid
      type AllErrorsOr[A] = Validated[List[String], A]
      println("Validated --> " + Semigroupal[AllErrorsOr].product(
        Validated.invalid(List("Error 1")),
        Validated.invalid(List("Error 2")))
      )

      // Validated complements Either nicely. Between the two we have support for both of the common types of error
      // handling: fail-fast and accumulating.

      val v = Validated.Valid(123)
      // v: cats.data.Validated.Valid[Int] = Valid(123)
      val i = Validated.Invalid(List("Badness"))
      // i: cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))
      println("Validated: Valid1 --> " + v)
      println("Validated: Invalid1 --> " + i)

      // it is o􏰃en easier to use the valid and invalid smart constructors, which widen the return type to Validated:
      val v2 = Validated.valid[List[String], Int](123)
      // v: cats.data.Validated[List[String],Int] = Valid(123)
      val i2 = Validated.invalid[List[String], Int](List("Badness"))
      // i: cats.data.Validated[List[String],Int] = Invalid(List(Badness))
      println("Validated: Valid2 --> " + v2)
      println("Validated: Invalid2 --> " + i2)

      // As a third option we can import the valid and invalid extension methods from cats.syntax.validated:
      import cats.syntax.validated._ // for valid and invalid
      println("Validated: ValidExt --> " + 123.valid[List[String]])
      println("Validated: InvalidExt --> " + List("Badness").invalid[Int])

      // As a fourth option we can use pure and raiseError from cats.syntax.applicative and cats.syntax.applicativeError
      // respectively:
      import cats.syntax.applicative._ // for pure
      import cats.syntax.applicativeError._ // for raiseError
      type ErrorsOr[A] = Validated[List[String], A]
      println("Validated: Pure --> " + 123.pure[ErrorsOr])
      println("Validated: RaiseError --> " + List("Badness").raiseError[ErrorsOr, Int])

      // create them from Exceptions, as well as instances of Try, Either, and Option
      println("Validated: catchOnly --> " + Validated.catchOnly[NumberFormatException]("foo".toInt))
      println("Validated: catchNonFatal --> " + Validated.catchNonFatal(sys.error("Badness")))
      println("Validated: fromTry --> " + Validated.fromTry(scala.util.Try("foo".toInt)))
      println("Validated: fromTry Valid --> " + Validated.fromTry(scala.util.Try("90".toInt)))
      println("Validated: fromEither --> " + Validated.fromEither[String, Int](Left("Badness")))
      println("Validated: fromOption --> " + Validated.fromOption[String, Int](None, "Badness"))
    }

    def combiningInstancesOfValidated = {
      import cats.instances.string._ // for Semigroup
      import cats.Semigroupal
      import cats.data.Validated
      type AllErrorsOr[A] = Validated[String, A]
      println(Semigroupal[AllErrorsOr])

      import cats.syntax.validated._
      import cats.syntax.semigroupal._

      import cats.syntax.apply._ // for tupled
      case class Errors[A](err1: AllErrorsOr[A], err2: AllErrorsOr[A])

      println("Validated errors --> " + (
        "Error 1".invalid[Int],
        "Error 2".invalid[Int]
      ).tupled)

      import cats.syntax.semigroup._
      import cats.instances.int._
      import cats.instances.list._

      println("Validation errors with semigroup |+| --> " + Semigroupal.map3(
        Validated.valid[List[String], Int](90),
        Validated.invalid[List[String], Int](List("Error")),
        Validated.valid[List[String], Int](99))
      (_ |+| _ |+| _))

      import cats.instances.vector._ // for Semigroupal
      println("Validation errors with Vectors --> " + (
        Vector(404).invalid[Int],
        Vector(500).invalid[Int]
      ).tupled)

      // The cats.data package also provides the NonEmptyList and NonEmptyVector types that prevent us failing without
      // at least one error:
      import cats.data.NonEmptyVector
      println("Validation errors with NonEmptyVector --> " + (
        NonEmptyVector.of("Error 1").invalid[Int],
        NonEmptyVector.of("Error 2").invalid[Int]
      ).tupled)
    }

    def methodsOfValidated = {
      import cats.syntax.validated._
      println("Methods of Validated (map)--> " + 123.valid[List[String]].map(_ * 100))
      println("Methods of Validated (leftMap) --> " + "?".invalid.leftMap(_.toString))
      println("Methods of Validated (bimap with valid) --> " + 123.valid[String].bimap(_ + "!", _ * 100))
      println("Methods of Validated (bimap with invalid) --> " + "?".invalid[Int].bimap(_ + "!", _ * 100))

      // The type signature of andThen is identical to that of flatMap, but it has a different name because it is not a
      // lawful implementation with respect to the monad laws:
      println("Methods of Validated (andThen)--> " + 32.valid.andThen { a =>
        10.valid.map { b =>
          a + b
        }
      })

      import cats.syntax.either._ // for toValidated
      println("Methods of Validated (Either1)--> " + "Badness".invalid[Int])
      // res22: cats.data.Validated[String,Int] = Invalid(Badness)
      println("Methods of Validated (toEither)--> " + "Badness".invalid[Int].toEither)
      println("Methods of Validated (toValidated)--> " + "Badness".invalid[Int].toEither.toValidated)
      // res23: Either[String,Int] = Left(Badness)
      // res24: cats.data.Validated[String,Int] = Invalid(Badness)
      println("Methods of Validated (ensure)--> " + -90.valid[String].ensure("Negative!")(_ > 0))
      println("Methods of Validated (getOrElse)--> " + "fail".invalid[Int].getOrElse(0))
      println("Methods of Validated (fold)--> " + "fail".invalid[Int].fold(_ + "!!!", _.toString))
    }

    createInstancesOfValidated
    combiningInstancesOfValidated
    methodsOfValidated
  }

  def excerciseFormValidation = {

    import cats.syntax.either._
    import cats.data.Validated
    type FormData = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]

    case class User(name: String, age: Int)
    def getValue(fieldName: String)(form: Map[String, String]): Either[List[String], String] =
      form.get(fieldName).toRight(List(s"$fieldName not specified"))

    def parseInt(fieldName: String)(data: String): Either[List[String], Int] = Either.catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List(s"$fieldName must be an integer"))

    def nonBlank(fieldName: String)(data: String): Either[List[String], String] =
      Right[List[String], String](data).
        ensure(List(s"$fieldName cannot be blank"))(_.nonEmpty)

    def nonNegative(fieldName: String)(data: Int): Either[List[String], Int] =
      Right[List[String], Int](data)
        .ensure(List(s"$fieldName should not be negative"))(_ > 0)

    def readName(form: Map[String, String]) =
      getValue("name")(form)
        .flatMap(nonBlank("name"))

    def readAge(form: Map[String, String]) =
      getValue("age")(form)
        .flatMap(nonBlank("age"))
        .flatMap(parseInt("age"))
        .flatMap(nonNegative("age"))

    import cats.syntax.either._
    import cats.instances.list._
    import cats.syntax.apply._ // for tupled and mapN

    def readUser(form: Map[String, String]): Validated[List[String], User] =
      (readName(form).toValidated,
        readAge(form).toValidated
      ).mapN(User.apply)

    val form = Map("name" -> "", "age" -> "-90")

    println("Excercise Form Validation1 --> " + readUser(Map("name" -> "Dave", "age" -> "37")))
    println("Excercise Form Validation2 --> " + readUser(Map("age" -> "-1")))
  }

}
