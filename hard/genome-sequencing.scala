import math._
import scala.util._




object Solution extends App {
    class MultipliableList[T](l: List[T]) {
        def *(n: Int) = (1 to n).flatMap(_=>l).toList
    }
    implicit def list2multipliable[T](l: List[T]) = new MultipliableList[T](l)
    def perms[T](input : List[T], n : Int) : List[List[T]] = {
    require(input.length > 0 && n > 0)
        n match {
            case 1 => for (el <- input) yield List(el)
            case _ => for (el <- input; perm <- perms(input, n - 1)) yield el :: perm
        }
    }

    val n = readInt
    var subseqs:List[String] = Nil
    for(i <- 0 until n) subseqs = subseqs :+ readLine
    val maxLen = subseqs.map(_.size).sum
    Console.err.println("Subseqs: " + subseqs.mkString(" "))
    var letters:String = ""
    for (subseq <- subseqs) letters = letters ++ subseq
    letters = letters.distinct
    letters = letters * (ceil(maxLen.toDouble/letters.size.toDouble)).toInt
    Console.err.println("Letters: " + letters.mkString(" "))


    for (len <- 1 to maxLen) {
        for (candidate <- perms(letters.toList, len).map(_.mkString)) {
            // Console.err.println("Candidate: " + candidate)
            if (subseqs.forall(s => candidate.containsSlice(s.toList))) {
                println(len)
                System.exit(0)
            }
        }

    }
}
