package hashcode

case class Point(row:Int, col:Int, height: Int) {
  import Main.problem._

  def validate(): Unit = {
    require(col< nbCols)
    require(col>=0)
    require(height< nbHeights)
    require(height>=0)
  }

  def addHeight(h: Int) = this.copy(height = height+h)
  def addVector(v:Vector) = this.copy(row= row + v.dr, col = (col + v.dc) % nbCols)
  def isLost = row < 0 || row >= nbRows
  //Method for cities
  def inZone(ballon: Point) = ballon.height > 0 && {
    val cdist = (ballon.col - col).abs min (nbCols - (ballon.col - col).abs)
    val rdist = ballon.row - row
    cdist*cdist + rdist*rdist <= radius*radius
  }

  def inAnyZone(balloons: Iterable[Point]) = balloons.exists(inZone)
}

case class Vector(dr:Int, dc: Int)

case class Problem(nbRows:Int, nbCols: Int, nbHeights:Int,
                   nbCells:Int, radius:Int, nbBallons:Int, nbTurns:Int,
                   startPoint: Point, targetCells: List[Point], winds: Map[Point, Vector]) {

}

