/*
 * Copyright (C) 2014 LAMP/EPFL
 * Copyright (C) 2014 KTH Royal Institute of Technology
 *
 * @author Philipp Haller
 * @author Heather Miller
 */
package selfassembly

/**
 * Simple representation of a type (corresponds to an "applied type" in Scala).
 */
case class TypeRep(typename: String, typeargs: List[TypeRep]) {
  override def toString =
    typename + (if (typeargs.isEmpty) "" else typeargs.mkString("[", ",", "]"))
}

object TypeRep {
  // the delimiters in an applied type
  private val delims = List(',', '[', ']')

  /* Parse an applied type.
   *
   * @param  s the string that is parsed
   * @return   a pair with the parsed applied type and the remaining string.
   */
  def parse(s: String): (TypeRep, String) = {
    // shape of `s`: fqn[at_1, ..., at_n]
    val (typename, rem) = s.span(!delims.contains(_))

    if (rem.isEmpty || rem.startsWith(",") || rem.startsWith("]")) {
      (TypeRep(typename, List()), rem)
    } else { // parse type arguments
      var typeArgs  = List[TypeRep]()
      var remaining = rem

      while (remaining.startsWith("[") || remaining.startsWith(",")) {
        remaining = remaining.substring(1)
        val (nextTypeRep, rem) = parse(remaining)
        typeArgs = typeArgs :+ nextTypeRep
        remaining = rem
      }

      (TypeRep(typename, typeArgs), if (remaining.startsWith("]")) remaining.substring(1) else remaining)
    }
  }

}
