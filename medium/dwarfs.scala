import math._
import scala.util._

object Solution extends App {
    val n = readInt

    var edges: List[(Int,Int)] = Nil
    for(i <- 0 until n) {
        val Array(x, y) = for(i <- readLine split " ") yield i.toInt
        edges = (x,y) :: edges
    }
    val nodes = (edges.map(_._1) ++ edges.map(_._2)).distinct
    val paths = for (x <- nodes; y <- nodes if (x != y)) yield path(x,y,edges,nodes,2,edges)
    println(paths.max)

    def path(x:Int, y:Int, edges:List[(Int,Int)], nodes:List[Int], count:Int, allEdges:List[(Int,Int)]): Int = {
        if (!(nodes.contains(x) || nodes.contains(y)))
            0
        else {
            val neigh = edges.filter(t => (edges.contains((x,t._1)) && t._1 != y)).map(t => t._1).distinct
            val paths = neigh.map(t => path(t, y, edges.filter(t => t._1 != x), nodes, count+1, allEdges))
            if (allEdges.contains(x,y)) {
                if (paths.isEmpty) count else max(paths.max,count)
            }
            else
                if (paths.isEmpty) 0  else paths.max
        }
    }
}
