import math._
import scala.util._

object Player extends App {
    // lightx: the X position of the light of power
    // lighty: the Y position of the light of power
    // initialtx: Thor's starting X position
    // initialty: Thor's starting Y position
    val Array(lightx, lighty, initialx, initialy) = for(i <- readLine split " ") yield i.toInt

    var thorx = initialx
    var thory = initialy
    // game loop
    while(true) {
        val remainingturns = readInt
        // Get directions
        val dirx = if (thorx > lightx) "W" else if (thorx < lightx) "E" else ""
        val diry = if (thory > lighty) "N" else if (thory < lighty) "S" else ""
        // Update thor coords
        if (dirx == "W") thorx = thorx - 1 else if (dirx == "E") thorx = thorx + 1
        if (diry == "N") thory = thory - 1 else if (diry == "S") thory = thory + 1

        println(s"$diry$dirx") // A single line providing the move to be made: N NE E SE S SW W or NW
    }
}
