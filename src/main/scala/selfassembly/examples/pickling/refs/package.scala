/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 Typesafe Inc. <http://www.typesafe.com>
 */
package selfassembly.examples.pickling

package object shareEverything {
  implicit object ShareEverything extends refs.ShareEverything
}

package object shareNothing {
  implicit object ShareNothing extends refs.ShareNothing
}

package refs {
  final class Ref

  sealed trait Share
  object Share {
    implicit object ShareNonPrimitives extends ShareNonPrimitives
  }

  sealed trait ShareNonPrimitives extends Share
  sealed trait ShareEverything extends Share
  sealed trait ShareNothing extends Share
}
