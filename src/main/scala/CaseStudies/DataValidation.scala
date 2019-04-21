package CaseStudies

object DataValidation {

  import cats.Semigroup
  import cats.syntax.either._
  import cats.syntax.semigroup._
  import cats.data.Validated

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(value: A): Either[E, A] = func(value)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { value =>
        (this (value), that(value)) match {
          case (Left(a), Left(b)) => (a |+| b).asLeft
          case (Left(a), Right(b)) => a.asLeft
          case (Right(a), Left(b)) => b.asLeft
          case (Right(a), Right(b)) => value.asRight
        }
      }

    // other methods...
  }

  def validate = {
    import cats.instances.list._ // for Semigroup
    val a: CheckF[List[String], Int] =
      CheckF { v =>
        if (v > 2) v.asRight
        else List("Must be > 2").asLeft
      }
    val b: CheckF[List[String], Int] =
      CheckF { v =>
        if (v < -2) v.asRight
        else List("Must be < -2").asLeft
      }
    val check: CheckF[List[String], Int] =
      a and b

    println(check(1))

    val a1: CheckF[Nothing, Int] =
      CheckF(v => v.asRight)
    val b1: CheckF[Nothing, Int] =
      CheckF(v => v.asRight)

    //a1 and b1
  }

  import cats.Semigroupal
  import cats.syntax.functor._
  import cats.Functor
  import cats.syntax.semigroupal._
  import cats.Monoid
  import cats.syntax.apply._
  import cats.data.Validated._

  // Algebraic Types
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    def apply(a: A)(implicit i: Semigroup[A], j: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        Semigroupal.map2(left(a), right(a))(_ |+| _)
      case Or(left, right) =>
        left(a) match {
          case Valid(m) => Valid(m)
          case Invalid(m) => right(a) match {
            case Valid(n) => Valid(n)
            case Invalid(n) => Invalid(m |+| n)
          }
        }
    }
  }

  final case class And[E, A](
                              left: Check[E, A],
                              right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](
                             left: Check[E, A],
                             right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](
                               func: A => Validated[E, A]) extends Check[E, A]

  def validateAlgebraic = {
    import cats.syntax.semigroup._
    import cats.instances.int._
    import cats.instances.list._

    val a: Check[List[String], Int] =
      Pure { v =>
        if (v > 2) Validated.valid[List[String], Int](v)
        else Validated.invalid[List[String], Int](List("Must be > 2"))
      }
    val b: Check[List[String], Int] =
      Pure { v =>
        if (v < -2) Validated.valid[List[String], Int](v)
        else Validated.invalid[List[String], Int](List("Must be < -2"))
      }
    val check: Check[List[String], Int] =
      a and b

    println(check(20))

    println((a or b).apply(0))
    println((a or b).apply(3))

    //    val a1: Check[Nothing, Int] =
    //      Pure(v => Validated.valid[Nothing,Int](v))
    //    val b1: Check[Nothing, Int] =
    //      Pure(v => Validated.valid[Nothing,Int](v))

    //    println(a1 and b1 apply  10)
  }

  def main(args: Array[String]): Unit = {
    validate
    println("-- Algebraic ---")
    validateAlgebraic
  }

}

object DataValidationWithPredicates {

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._ // for mapN
  import cats.data.Validated._ // for Valid and Invalid
  sealed trait Predicate[E, A] {
    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(a1) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
  }

  final case class And[E, A](
                              left: Predicate[E, A],
                              right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](
                             left: Predicate[E, A],
                             right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](
                               func: A => Validated[E, A]) extends Predicate[E, A]


  import cats.Semigroup
  import cats.data.Validated

  sealed trait Check[E, A, B] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = {
      FlatMap1(this, f)
    }

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
  }

  final case class AndThen[E, A, B, C](
                                        checkThis: Check[E, A, B],
                                        checkThat: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = checkThis(a).withEither(_.flatMap(b => checkThat(b).toEither))
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      Pure1(pred)
  }

  final case class Map[E, A, B, C](
                                    check: Check[E, A, B],
                                    func: B => C) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).map(func)
  }

  final case class Pure1[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
  }

  final case class FlatMap1[E, A, B, C](
                                         check: Check[E, A, B],
                                         func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in) match {
      case Valid(m) => func(m)(in)
      case Invalid(m) => invalid[E, C](m)
    }
  }

  final case class FlatMap[E, A, B, C](
                                        check: Check[E, A, B],
                                        func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  case class Model(i: Int)

  def main(args: Array[String]): Unit = {
    import cats.instances.list._
    import cats.instances.int._
    import cats.data.Validated._
    //Check(Pure[Int,List[String]](x => valid[List[String], Int](10)))(10)
    println(Check[List[String], Int](Pure(x => invalid[List[String], Int](List("abc"))))(10))
    println(Check[List[String], Int](Pure(x => valid[List[String], Int](10))).map(Model)(10))
    val f = (a: Int) => Check[List[String], Int](Pure(x => valid[List[String], Int](10))).map(Model)
    println(Check[List[String], Int](Pure(x => valid[List[String], Int](10)))
      .flatMap(f)(10))

    val app = f(90) andThen Check[List[String], Model](Pure(x => valid[List[String], Model](Model(99)))).map(s => "uiui")
    val app2 = f(90) andThen Check[List[String], Model](Pure(x => valid[List[String], Model](Model(99)))).map(_.i.toString)
    val app3 = f(90) andThen Check[List[String], Model](Pure(x => invalid[List[String], Model](List("andThen error")))).map(_.i.toString)

    println(app(80))
    println(app2(80))

    println(app3(80))

    val c1 = Check[List[String], Int](Pure(x => invalid[List[String], Int](List("pre andThen error")))).map(Model)
    val app4 = c1 andThen Check[List[String], Model](Pure(x => valid[List[String], Model](Model(99)))).map(_.i.toString)
    println(app4(80))
  }
}

object DataValidationWithoutKleisli {

  import cats.data.Kleisli
  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._ // for Valid and Invalid import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._ // for mapN
  import cats.syntax.validated._ // for valid and invalid

  sealed trait Predicate[E, A] {

    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(a1) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2) => Valid(a)
              case Invalid(e2) => Invalid(Semigroup.combine(e1, e2))
            }
        }
    }
  }

  object Predicate {

    final case class And[E, A](
                                left: Predicate[E, A],
                                right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](
                               left: Predicate[E, A],
                               right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](
                                 func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if (fn(a)) a.valid else err.invalid)
  }

  sealed trait Check[E, A, B] {

    import Check._

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C]
    =
      Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]) =
      FlatMap[E, A, B, C](this, f)

    def andThen[C](next: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, next)
  }

  object Check {

    final case class Map[E, A, B, C](
                                      check: Check[E, A, B],
                                      func: B => C) extends Check[E, A, C] {
      def apply(a: A)
               (implicit s: Semigroup[E]): Validated[E, C] =
        check(a) map func
    }

    final case class FlatMap[E, A, B, C](
                                          check: Check[E, A, B],
                                          func: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(a: A)
               (implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }

    final case class AndThen[E, A, B, C](
                                          check: Check[E, A, B],
                                          next: Check[E, B, C]) extends Check[E, A, C] {
      def apply(a: A)
               (implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => next(b).toEither))
    }

    final case class Pure[E, A, B](
                                    func: A => Validated[E, B]) extends Check[E, A, B] {
      def apply(a: A)
               (implicit s: Semigroup[E]): Validated[E, B] =
        func(a)
    }

    final case class PurePredicate[E, A](
                                          pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(a: A)
               (implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      PurePredicate(pred)

    def apply[E, A, B]
    (func: A => Validated[E, B]): Check[E, A, B] =
      Pure(func)
  }

  def validationsForCreateUser = {
    import cats.data.{NonEmptyList, Validated}
    type Errors = NonEmptyList[String]

    def error(s: String): NonEmptyList[String] =
      NonEmptyList(s, Nil)

    def longerThan(n: Int): Predicate[Errors, String] =
      Predicate.lift(
        error(s"Must be longer than $n characters"),
        str => str.size > n)

    val alphanumeric: Predicate[Errors, String] =
      Predicate.lift(
        error(s"Must be all alphanumeric characters"),
        str => str.forall(_.isLetterOrDigit))

    def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

    def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

    val check = Check(alphanumeric and longerThan(10))
    println(check("dfvijbdfvibdfvdfv"))

    import cats.data.{NonEmptyList, Validated}
    import cats.syntax.apply._ // for mapN
    import cats.syntax.validated._ // for valid and invalid

    // A username must contain at least four characters
    // and consist entirely of alphanumeric characters
    val checkUsername: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

    // An email address must contain a single `@` sign. // Split the string at the `@`.
    // The string to the left must not be empty.
    // The string to the right must be
    // at least three characters long and contain a dot.
    val splitEmail: Check[Errors, String, (String, String)] = Check(_.split('@') match {
      case Array(name, domain) =>
        (name, domain).validNel[String]
      case other =>
        "Must contain a single @ character".
          invalidNel[(String, String)]
    })
    val checkLeft: Check[Errors, String, String] =
      Check(longerThan(0))
    val checkRight: Check[Errors, String, String] =
      Check(longerThan(3) and contains('.'))

    val joinEmail: Check[Errors, (String, String), String] = Check { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }
    val checkEmail: Check[Errors, String, String] =
      splitEmail andThen joinEmail

    final case class User(username: String, email: String)
    def createUser(
                    username: String,
                    email: String): Validated[Errors, User] = (checkUsername(username), checkEmail(email)).mapN(User)

    println(createUser("Noel", "noel@underscore.io"))
    println(createUser("", "dave@underscore@io"))
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.list._
    import cats.instances.int._
    import cats.instances.string._
    import cats.data.Validated._

    println(Check(Predicate[List[String], Int](a => invalid(List("stabiliser"))))(1))

    validationsForCreateUser

    val step1: Kleisli[List, Int, Int] =
      Kleisli(x => List(x + 1, x - 1))
    val step2: Kleisli[List, Int, Int] =
      Kleisli(x => List(x, -x))
    val step3: Kleisli[List, Int, Int] =
      Kleisli(x => List(x * 2, x / 2))

    val pipeline = step1 andThen step2 andThen step3

    println(pipeline(10))

  }
}

object DataValidationWithKleisli {

  import cats.data.Kleisli
  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._ // for Valid and Invalid import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._ // for mapN
  import cats.syntax.validated._ // for valid and invalid

  sealed trait Predicate[E, A] {

    import Predicate._

    def run(implicit s: Semigroup[E]): A => Either[E, A]
    = (a: A) => this (a).toEither

    def and(that: Predicate[E, A]): Predicate[E, A] =
      And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(a1) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2) => Valid(a)
              case Invalid(e2) => Invalid(Semigroup.combine(e1, e2))
            }
        }
    }
  }

  object Predicate {

    final case class And[E, A](
                                left: Predicate[E, A],
                                right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](
                               left: Predicate[E, A],
                               right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](
                                 func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if (fn(a)) a.valid else err.invalid)
  }

  import cats.data.{Kleisli, NonEmptyList, Validated}
  import cats.syntax.apply._ // for mapN

  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)

  // remember that the run method on Predicate takes an implicit parameter. If you call aPredicate.run(a) it will try
  // to pass the implicit parameter explicitly. If you want to create a function from a Predicate and immediately apply
  // that function, use aPredicate.run.apply(a)
  def checkPredicate[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli(pred.run)

  def validationsForCreateUser = {

    def error(s: String): NonEmptyList[String] =
      NonEmptyList(s, Nil)

    def longerThan(n: Int): Predicate[Errors, String] =
      Predicate.lift(
        error(s"Must be longer than $n characters"),
        str => str.size > n)

    val alphanumeric: Predicate[Errors, String] =
      Predicate.lift(
        error(s"Must be all alphanumeric characters"),
        str => str.forall(_.isLetterOrDigit))

    def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

    def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

    // A username must contain at least four characters
    // and consist entirely of alphanumeric characters
    val checkUsername = checkPredicate(longerThan(3) and alphanumeric)

    // An email address must contain a single `@` sign. // Split the string at the `@`.
    // The string to the left must not be empty.
    // The string to the right must be
    // at least three characters long and contain a dot.
    val splitEmail = check[String, (String, String)](_.split('@') match {
      case Array(name, domain) =>
        Right((name, domain))
      case other =>
        Left(NonEmptyList(
          "Must contain a single @ character", Nil))
    })

    val checkLeft =
      checkPredicate(longerThan(0))
    val checkRight =
      checkPredicate(longerThan(3) and contains('.'))

    // For Semigroupal[Either]
    import cats.instances.either._
    val joinEmail: Check[(String, String), String] =
      check {
        case (l, r) =>
          (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
      }

    import cats.instances.either._
    val checkEmail =
      splitEmail andThen joinEmail

    final case class User(username: String, email: String)
    def createUser(
                    username: String,
                    email: String): Either[Errors, User] =
      (checkUsername(username), checkEmail(email)).mapN(User)

    println(createUser("Noel", "noel@underscore.io"))
    println(createUser("", "dave@underscore@io"))
  }


  def main(args: Array[String]): Unit = {
    println("<--- Kleisli magic ---->")
    validationsForCreateUser
  }

}