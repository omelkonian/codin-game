import math._
import scala.util._

object Solution extends App {
    val n = readInt
    val c = readInt
    var budgets: List[Int] = Nil
    for(i <- 0 until n) budgets = budgets :+ readInt
    if (budgets.sum < c) {
        println("IMPOSSIBLE")
        System.exit(0)
    }
    var contributions: List[(Int,Int)] = List.fill(n)(0).zip(budgets)
    var count = c
    while (count > 0) {
        contributions = contributions.map(b =>
            if (b._1 < b._2 && count != 0) {
                count -= 1
                (b._1+1, b._2)
            }
            else b
        )
    }
    println(contributions.map(_._1).sorted.mkString("\n"))
}
