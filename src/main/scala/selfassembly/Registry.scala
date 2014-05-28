/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package selfassembly

trait Registry[TC[_]] {
  def register(clazz: Class[_], instance: TC[_]): Unit
  def get(clazz: Class[_]): Option[TC[_]]
}

class SimpleRegistry[TC[_]] extends Registry[TC] {
  private var m = Map[Class[_], TC[_]]()

  def register(clazz: Class[_], instance: TC[_]): Unit =
    m += (clazz -> instance)

  def get(clazz: Class[_]): Option[TC[_]] =
    m.get(clazz)
}
