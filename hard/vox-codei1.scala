import math._
import scala.util._


object Player extends App {
    def valid(grid:Array[Array[Char]], b:(Int,Int), n:(Int,Int), l:(Int,Int)): Boolean = {
        inRange(grid,b,n) && unblocked(grid,b,n,l)
    }
    def inRange(grid:Array[Array[Char]], bombPos:(Int,Int), nodePos:(Int,Int)): Boolean = {
        val (x,y) = bombPos ; val (i,j) = nodePos
        return (x==i || y==j) && abs(x-i) < 4 && abs(y-j) < 4 && grid(x)(y) == '@'
    }
    def unblocked(grid:Array[Array[Char]], bombPos:(Int,Int), nodePos:(Int,Int), limits:(Int,Int)): Boolean = {
        val (x,y) = bombPos ; val (i,j) = nodePos
        val (xmin,xmax,ymin,ymax) = (min(x,i), max(x,i), min(y,j), max(y,j))
        val between =
            (for (i <- 0 until limits._1; j <- 0 until limits._2) yield (i,j))
            .filter {
                case(k,l) => ((xmin to xmax).contains(k)) && ((ymin to ymax).contains(l)) && grid(k)(l)=='#'
            }
        return between.size == 0
    }
    def getBombs(grid:Array[Array[Char]], pos:(Int,Int), limits:(Int,Int)): Int = {
        val (x,y) = pos
        val positions = for (i <- 0 until h; j <- 0 until w) yield (i,j)
        return positions.filter {
            case(i,j) => valid(grid, (i,j), (x,y), limits)
        }.size
    }

    val Array(w, h) = for(i <- readLine split " ") yield i.toInt
    val grid = Array.ofDim[Char](h,w)
    for(i <- 0 until h) grid(i) = readLine.toArray

    // game loop
    while(true) {
        val Array(rounds, bombs) = for(i <- readLine split " ") yield i.toInt
        // Get possible bomb placements
        val positions =
            (for (i <- 0 until h; j <- 0 until w if (!List('#','@').contains(grid(i)(j))))
            yield ((i,j),getBombs(grid,(i,j),(h,w))))
                .filter(_._2 > 0)
                .sortWith(_._2 > _._2)
        Console.err.println(positions.mkString(" "))
        if (positions.isEmpty) println("WAIT")
        else {
            // Choose most dectructing one
            val (x,y) = positions.head._1
            println(s"$y $x")
            // Remove bombs
            (for (i <- 0 until h; j <- 0 until w) yield (i,j))
                .filter{ case(i,j) => valid(grid,(i,j),(x,y),(h,w)) }
                .map{ case(i,j) => grid(i)(j) = '.' }
        }
    }
}
