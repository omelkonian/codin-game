import math._
import scala.util._

object Solution extends App {
    val r = readInt
    val l = readInt
    val line0 = List(r)

    def construct(line: List[Int], count: Int): Unit = {
        val nextLine = line.foldRight(List[List[Int]]()) {
            (e, l) => l match {
                case (`e` :: xs) :: fs => (e :: e :: xs) :: fs
                case _ => List(e) :: l
            }
        }.map(x => (x.size, x.head))
        val flat = nextLine.flatMap(x => List(x._1,x._2))
        if (count == 1) {
            val lastv = flat.last
            line.dropRight(1).foreach { x => print(x + " ") }
            println(lastv)
        }
        else
            construct(flat, count-1)
    }

    construct(line0, l)

}
