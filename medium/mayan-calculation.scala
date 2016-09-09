import math._
import scala.util._
import scala.collection.mutable.Map

object Solution extends App {
    def fromDecimal (dec: BigInt, base: Int) : List[Int] =
        if (dec==0L) Nil else (dec % base).toInt :: fromDecimal (dec/base, base)

    val Array(l, h) = for(i <- readLine split " ") yield i.toInt

    var list: Array[String] = Array()
    for(i <- 0 until h) list = list :+ readLine

    // Construct maps
    val map: Map[String,Int] = Map()
    for (n <- 0 to 19) {
        val letter = Array.ofDim[Char](h,l)
        for (c <- 0 until h) letter(c) = list(c).toArray.slice(n*l, n*l + l)
        map += (letter.map(_.mkString).mkString -> n)
    }

    // Convert input to numbers
    val p1 = readInt/h - 1
    var num1: BigInt = 0
    for(power <- p1 to 0 by -1) {
        var n1: String = ""
        for (j <- 0 until h) n1 = n1 ++ readLine
        num1 += map(n1)*pow(20,power).toInt
    }

    val p2 = readInt/h - 1
    var num2: BigInt = 0
    for(power <- p2 to 0 by -1) {
        var n2: String = ""
        for (j <- 0 until h) n2 = n2 ++ readLine
        num2 += map(n2)*pow(20,power).toInt
    }

    // Get result
    val operation = readLine
    val result: BigInt = operation match {
        case "+" => num1 + num2
        case "-" => num1 - num2
        case "*" => num1 * num2
        case "/" => num1 / num2
    }

    // Convert to base 20
    val base20 = if (result == 0) List(0) else fromDecimal(result, 20).reverse

    // Output
    val revMap: Map[Int,String] = map.map(_.swap)
    for (num <- base20)
        println(revMap(num).grouped(h).mkString("\n"))
}
