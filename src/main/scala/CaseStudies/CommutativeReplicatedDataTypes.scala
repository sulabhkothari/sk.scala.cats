package CaseStudies

object CommutativeReplicatedDataTypes {

  import cats.kernel.CommutativeMonoid

  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A

    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intInstance: BoundedSemiLattice[Int] =
      new BoundedSemiLattice[Int] {
        def combine(a1: Int, a2: Int): Int =
          a1 max a2

        val empty: Int =
          0
      }

    implicit def setInstance[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] =
        a1 union a2

      val empty: Set[A] =
        Set.empty[A]
    }
  }

  import cats.instances.list._ // for Monoid
  import cats.instances.map._ // for Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._ // for combineAll

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)
                 (implicit m: CommutativeMonoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])
             (implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])
             (implicit m: CommutativeMonoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter

    // including CommutativeMonoid[Map[K,V]] stops merge from using BoundedSemiLattice[V] and adds instead of max
    implicit def mapInstance[K, V] //(implicit g: CommutativeMonoid[Map[K, V]])
    : GCounter[Map, K, V] =
      new GCounter[Map, K, V] {
        def increment(map: Map[K, V])(key: K, value: V)
                     (implicit m: CommutativeMonoid[V]): Map[K, V] = {
          val total = map.getOrElse(key, m.empty) |+| value
          map + (key -> total)
        }

        // This doesn't compile if import cats.instances.map._ is removed
        def merge(map1: Map[K, V], map2: Map[K, V])
                 (implicit b: BoundedSemiLattice[V]): Map[K, V] =
          map1 |+| map2

        def total(map: Map[K, V])
                 (implicit m: CommutativeMonoid[V]): V =
          map.values.toList.combineAll
      }

    def gcounterInstance[F[_, _], K, V]
    (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]) =
      new GCounter[F, K, V] {
        def increment(f: F[K, V])(key: K, value: V)
                     (implicit m: CommutativeMonoid[V]): F[K, V] = {
          val total = f.getOrElse(key, m.empty) |+| value
          f.put(key, total)
        }

        def merge(f1: F[K, V], f2: F[K, V])
                 (implicit b: BoundedSemiLattice[V]): F[K, V] =
          f1 |+| f2

        def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
      }
  }

  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  implicit val mapInstance: KeyValueStore[Map] =
    new KeyValueStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] =
        f.get(k)

      override def getOrElse[K, V](f: Map[K, V])
                                  (k: K, default: V): V =
        f.getOrElse(k, default)

      def values[K, V](f: Map[K, V]): List[V] =
        f.values.toList
    }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)
           (implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)
                 (implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }

  def main(args: Array[String]): Unit = {
    //import cats.instances.int._ // for Monoid
    import BoundedSemiLattice._
    //import cats.instances.map.catsKernelStdCommutativeMonoidForMap
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)
    val counter = GCounter[Map, String, Int]
    val merged = counter.merge(g1, g2)

    println(g1)
    println(g2)
    println(merged)
    // merged: Map[String,Int] = Map(a -> 7, b -> 5)
    val total = counter.total(merged)
    println(total)

    val counter2 = GCounter.gcounterInstance[Map, String, Int]
    println(counter2.merge(g1, g2))
    //println("Why this difference?")
    // CommutativeMonoid[F[K, V]] in implicit scope looks for Commutative Monoid of Int in implicit scope which it found
    // via cats.instances.int._ (which is now commented). That's why it never used implicit for BoundedSemiLattice in
    // this case since it came in implicit scope later on via merge method parameter (by that time it already used
    // CommutativeMonoid[Int] from cats). Even if implicit BoundedSemiLattice parameter is removed from merge method, with
    // this approach it still works as it uses CommutativeMonoid[F[K,V]] to combine values.

    // imported BoundedSemiLattice instead resolved the issue because it brings a CommuntativeMonoid[Int] into scope
    // for CommutativeMonoid[Map[String,Int]]

    // In case of directly using Map without generalization, it uses implicit BoundedSemiLattice, which is resolved
    // automatically using its companion object. In this case, CommutativeMonoid for map is resolved at call site of |+|
    // by when BoundedSemiLattice[V] is available, which has already been summoned implicitly, to merge value of V.
    // That is why, it worked seamlessly even earlier. Removing implicit parameter BoundedSemiLattice for this approach,
    // breaks it, because map needs BoundedSemiLattice[V] to merge values, as there is no CommutativeMonoid for map in
    // implicit scope with this approach.

    // Assumption (Later Verified in ImplicitModelling):
    // Even if BoundedSemiLattice[V] is added to "gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F],
    // km: CommutativeMonoid[F[K, V]])", it won't work because when CommutativeMonoid[F[K,V]] is resolved,
    // BoundedSemiLattice[V] is still not in implicit scope (as both resolutions are happening at the same level)

    // Difference is because using map[k,v] without asking for its monoid makes it use monoids for k,v
    // and we have monoid for int in implicit scope

    import cats.Monoid
    // fails because of ambiguity in resolving implicits
    //    implicit val mInt = new Monoid[Int]{
    //      override def empty: Int = 90
    //
    //      override def combine(x: Int, y: Int): Int = x * y
    //    }
    val mi = implicitly[Monoid[Int]]
    val m1 = Map(1 -> 2)
    val m2 = Map(1 -> 20)

    println(implicitly[Monoid[Map[Int, Int]]].combine(m1, m2))

    println(implicitly[Monoid[Int]].combine(2, 4))
  }
}
