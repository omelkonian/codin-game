import math._
import scala.util._
import scala.collection.mutable._


object Solution extends App {
    // Memoization
    def memoize[I, O](f: I => O): collection.Map[I, O] = new HashMap[I, O]() {
      override def apply(key: I) = getOrElseUpdate(key, f(key))
    }

    // Input
    val n = readInt
    var l = new ListBuffer[(Int, Int)]()
    val s =
        (1 to n)
        .map(x => readLine split " ")
        .map {case Array(x, y) => (x.toInt, y.toInt) }
        .flatMap {case (x, y) => Array((x, y), (y, x))}
        .groupBy(_._1)
        .map {case entry => entry._1 -> entry._2.map(_._2)}

    lazy val getHeight: ((Int, Int)) => Int = memoize {
        case (cur: Int, par: Int) =>
            val children = (s get cur).get filterNot par.==
            val l = children.map(getHeight(_, cur))
            l.foldLeft(-1)(Math.max) + 1
    }

    println(s.keys.map(x => getHeight(x, -1)).reduceLeft(Math.min))
}
