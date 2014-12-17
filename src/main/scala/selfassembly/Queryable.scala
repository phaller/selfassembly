/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package selfassembly

/**
 * To handle cycles in a more local way, we require type classes
 * to extend the `Queryable` trait.
 */
trait Queryable[T, R] {
  // used for invocations that are not top-level
  def apply(visitee: T, visited: Set[Any]): R
}
