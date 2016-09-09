import math._
import scala.util._

object Solution extends App {
    val n = readInt
    val list = readLine.split(' ').map(_.toInt)

    var maxLoss = 0
    var maxValue = 0
    for (item <- list) {
        if (item > maxValue) maxValue = item
        else if (maxValue - item > maxLoss) maxLoss = maxValue - item
    }

    println(-maxLoss)
}
