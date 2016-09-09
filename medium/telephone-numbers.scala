import math._
import scala.util._


object Solution extends App {
    val n = readInt
    var telephones: List[String] = Nil
    var ans = 0
    for(i <- 0 until n) {
        val tel = readLine
        ans += tel.size - prefix(tel, telephones)
        telephones = tel :: telephones
    }
    println(ans)
    def longestCommonPrefix(a: String, b: String): Int = {
        var same = true
        var i = 0
        while(same && i < math.min(a.length, b.length)) {
            if(a.charAt(i) != b.charAt(i)) same = false
            else i += 1
        }
        i
    }
    def prefix(s: String, ls: List[String]): Int = {
        if (ls.isEmpty) 0 else ls.map(longestCommonPrefix(_, s)).max
    }
}
