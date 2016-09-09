import math._
import scala.util._

object Solution extends App {
    val n = readInt
    var ys:List[BigDecimal] = Nil
    var xs:List[BigDecimal] = Nil

    for(i <- 0 until n) {
        val Array(x, y) = for(i <- readLine split " ") yield BigDecimal(i)
        ys = y :: ys
        xs = x :: xs
    }
    xs = xs.sorted

    val maxx = xs.last - xs.head

    def median(s: Seq[BigDecimal]) = {
        val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
        if (s.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
    }
    val med = median(ys)

    val dis = ys.map(x => (x - med).abs)

    println((dis.sum + maxx).toBigInt)
}

