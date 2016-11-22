package monadics.instances

import monadics.structures.Monoid

trait map {

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
