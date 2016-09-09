import math._
import scala.util._
import scala.util.control.Breaks._


object Solution extends App {
    def printArr(array:Array[Array[Char]]) = {
        array foreach{case a => a foreach {b => Console.err.print(b.toString + " ")}; Console.err.print('\n')}
    }
    def getStart(array:Array[Array[Char]], w:Int, h:Int): (Int, Int) = {
        for (i <- 0 until h)
        for (j <- 0 until w)
            if (array(i)(j) == '@')
                return (i, j)
        (-1, -1)
    }
    def getTeleports(array:Array[Array[Char]], w:Int, h:Int) = {
        for {
            i <- 0 until h
            j <- 0 until w
            if array(i)(j) == 'T'
        }
        yield (i,j)
    }
    def getPos(pos:(Int,Int), dir:String):(Int,Int) = {
        val (x,y) = pos
        dir match {
            case "SOUTH" => (x+1,y)
            case "EAST"  => (x,y+1)
            case "NORTH" => (x-1,y)
            case "WEST"  => (x,y-1)
        }
    }
    def getDir(dir:String, array:Array[Array[Char]], pos:(Int,Int), priorities:Array[String]):String = {
        val (x,y) = pos;
        for (priority <- priorities) {
            val bool = priority match {
                case "SOUTH" => (grid(x+1)(y) != '#' && grid(x+1)(y) != 'X')
                case "EAST"  => (grid(x)(y+1) != '#' && grid(x)(y+1) != 'X')
                case "NORTH" => (grid(x-1)(y) != '#' && grid(x-1)(y) != 'X')
                case "WEST"  => (grid(x)(y-1) != '#' && grid(x)(y-1) != 'X')
            }
            if (bool) return priority
        }
        ""
    }

    var priorities = Array("SOUTH","EAST","NORTH","WEST")
    val Array(h, w) = for(i <- readLine split " ") yield i.toInt
    val grid = Array.ofDim[Char](h,w)
    for(i <- 0 until h) {
        grid(i) = readLine.toCharArray
    }
    printArr(grid)

    var (x,y) = getStart(grid, w, h)
    var teleports = getTeleports(grid, w, h)
    var moves = Nil: List[String]
    var beer = false
    var broken = false
    var dir = "SOUTH"
    var positions = Nil: List[((Int,Int),Char,String,Boolean)] // (Coords, Symbol, Direction)
    while (true) {
        val c = grid(x)(y)
        val position = ((x,y),c,dir,beer)
        if (positions.contains(position)) { println("LOOP") ; break }
        if (broken) broken = false
        else positions = positions :+ position
        // Console.err.print(s"[$x,$y]: $c")
        val (x2,y2) = getPos((x,y), dir)
        var c2 = grid(x2)(y2)
        if (beer && c2 == 'X') {
            c2 = ' ' ; broken = true
            positions = Nil
            grid(x2)(y2) = ' '
        }
        c2 match {
            case ' '|'@'|'B' =>
                Console.err.println(s"[$x2,$y2]: $c2")
                x=x2; y=y2
                if (c2 == 'B') beer = !beer ; Console.err.println("Beer!")
                moves = moves :+ dir
            case '#'|'X' =>
                dir = getDir(dir, grid, (x,y), priorities)
            case '$' =>
                Console.err.println("FINISH!!!")
                moves = moves :+ dir
                println(moves.mkString("\n"))
                break
            case 'I' =>
                Console.err.println(s"[$x2,$y2]: $c2")
                x=x2; y=y2
                priorities = priorities.reverse
                moves = moves :+ dir
            case 'S'|'E'|'N'|'W' =>
                Console.err.println(s"[$x2,$y2]: $c2")
                x=x2; y=y2
                moves = moves :+ dir
                dir = priorities.filter(t => t(0) == c2)(0)
            case 'T' =>
                Console.err.println(s"[$x2,$y2]: $c2")
                val (telx,tely) = teleports.filter(c => (c._1!=x2 || c._2!=y2)).head
                Console.err.println(s"Teleport! - [$telx, $tely]")
                positions = Nil
                x=telx; y=tely
                moves = moves :+ dir
        }
    }
}
