package hashcode

import scala.annotation.tailrec
import scala.collection.mutable

case class Point(row: Int, col: Int, height: Int) {

  import Main.problem._

  def validate(): Unit = {
    require(col < nbCols)
    require(col >= 0)
    require(height < nbHeights)
    require(height >= 0)
  }

  def addHeight(h: Int) = this.copy(height = height + h)
  def addVector(v: WindVector) = this.copy(row = row + v.dr, col = (col + v.dc + nbCols) % nbCols)

  def reachable = for {
    i <- -1 to 1
    p = addHeight(i)
    if p.height <= nbHeights && p.height >= 1
  } yield p

  def isLost = row < 0 || row >= nbRows

  //Method for cities
  def inZone(ballon: Point, problem: Problem) = ballon.height > 0 && {
    import problem._
    val cdist = (ballon.col - col).abs min (nbCols - (ballon.col - col).abs)
    val rdist = ballon.row - row
    cdist * cdist + rdist * rdist <= radius * radius
  }

  //def inAnyZone(balloons: Iterable[Point]) = balloons.exists(inZone)

  def to2d = Point2D(row, col)
}

case class WindVector(dr: Int, dc: Int) {
  val speed = math.sqrt(dr * dr + dc * dc)
}
case class Point2D(row: Int, col: Int)

case class Problem(nbRows: Int, nbCols: Int, nbHeights: Int,
                   nbCells: Int, radius: Int, nbBallons: Int, nbTurns: Int,
                   startPoint: Point, targetCells: Vector[Point], winds: Map[Point, WindVector]) {

  val connectedCitiesMap: Map[Point2D, Vector[Int]] = (for {
    row <- 0 until nbRows
    col <- 0 until nbCols
    p = Point(row, col, 1)
  } yield p.to2d -> targetCells.zipWithIndex.collect({ case (c, i) if c.inZone(p, this) => i })).toMap

  def connectedCount(balloons: Iterable[Point]) =
    balloons.toSet.flatMap({ b: Point => if (b.height > 0) connectedCitiesMap(b.to2d) else Set.empty }).size

  def successors(p:Point) = (p.height match {
    case 0 ⇒ p.addHeight(1) :: Nil
    case 1 ⇒ p :: p.addHeight(1) :: Nil
    case `nbHeights` ⇒ p :: p.addHeight(-1) :: Nil
    case _ ⇒ List(p, p.addHeight(1), p.addHeight(-1))
  }) map (p ⇒ p.addVector(winds(p))) filterNot (_.isLost)


  case class BFSStat(parent:Point, depth: Int, score: Int) {
    def addEdge(from :Point, to: Point) = BFSStat(from, depth+1, score + scoreAt(to))
  }

  object BFSStat { def initial(p:Point) = BFSStat(p,0,0) }

  def bfs(p:Point): Map[Point, BFSStat] = bfs(Vector(p), Map(p → BFSStat.initial(p)))

  @tailrec
  final def bfs(toVisit: Vector[Point], visited:Map[Point, BFSStat]=Map.empty): Map[Point, BFSStat] = {
    if (toVisit.isEmpty) visited
    else {
      val p = toVisit.head
      val stat = visited(p)
      val children = successors(p).filterNot(visited.contains)
      bfs(
        toVisit.tail ++ children,
        visited ++ children.map { child ⇒ child → stat.addEdge(p,child) }
      )
    }
  }

  def scoreAt(p: Point) = connectedCitiesMap(p.to2d).length

  def dfs(p:Point, level:Int = 0,
          visitingPath:List[Point] = List.empty, visitingSet: /*mutable.*/Set[Point]=/*mutable.*/Set.empty,
          visited: mutable.Set[Point] = mutable.Set.empty): List[List[Point]] =
    if (visitingSet(p))
      List(p :: visitingPath) //println("New cycle found: " + (p :: visitingPath) )
    else if (visited(p) || level==nbTurns)
      List.empty
    else {
      val succs = successors(p)
      val visitingSet1 = visitingSet + p
      //visitingSet += p
      val visitingPath1 = p :: visitingPath
      val r = succs flatMap { s ⇒ dfs(s, level+1, visitingPath1, visitingSet1, visited) }
     // visitingSet -= p
      visited += p
      r
    }

}
