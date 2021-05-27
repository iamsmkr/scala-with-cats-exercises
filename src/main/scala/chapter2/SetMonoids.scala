package chapter2

import cats.{Monoid, Semigroup}

object SetMonoids {

  // set union forms a monoid along with the empty set
  // we need to defne setUnionMonoid as a method rather than a value so we can accept the type parameter A
  implicit def setUnionMoniod[A](): Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }

  // set intersecton forms a semigroup, but doesn’t form a monoid because it has no identty element
  implicit def setIntersectionSemigroup[A](): Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
    }

  // set complement and diference are not associatve, so they cannot be considered for either monoids or semigroups

  // symmetric diference (the union less the intersecton) does form a monoid with the empty set
  implicit def symmetricDifferenceMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty: Set[A] = Set.empty[A]

      def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
    }
}
