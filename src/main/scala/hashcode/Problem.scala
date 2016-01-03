package hashcode

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

case class WindVector(dr: Int, dc: Int)
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

}
