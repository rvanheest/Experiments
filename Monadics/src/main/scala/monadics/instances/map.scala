package monadics.instances

import monadics.structures.{Equals, Monoid}

trait map {

  implicit def mapIsEquals[K, V](implicit valueEquals: Equals[V]): Equals[Map[K, V]] = {
    Equals.create((m1, m2) => m1.size == m2.size && m1.forall { case (k1, v1) => m1.get(k1).exists(valueEquals.equals(v1, _)) })
  }

  implicit def mapIsMonoid[K, V](implicit valueIsMonoid: Monoid[V]): Monoid[Map[K, V]] = Monoid.create(Map.empty[K, V]) {
    (map1, map2) => map2.foldLeft(map1) {
      case (map, (k, v)) => map.updated(k, map.get(k).map(xv => valueIsMonoid.combine(xv, v)).getOrElse(v))
    }
  }

  implicit class MapIsMonoid[K, V](map: Map[K, V])(implicit monoid: Monoid[Map[K, V]], valueIsMonoid: Monoid[V]) {
    def combine(other: Map[K, V]): Map[K, V] = monoid.combine(map, other)
  }
}

object map extends map
