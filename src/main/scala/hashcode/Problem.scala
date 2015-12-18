package hashcode

case class Point(row:Int, col:Int, height: Int) {
  import Main.problem._
  require(col< nbCols)
  require(col>=0)
  require(row< nbRows)
  require(row>=0)
  require(height< nbHeights)
  require(height>=0)

  def addHeight(h: Int) = this.copy(height = height+h)
  def addVector(v:Vector) = this.copy(row= row + v.dr, col = (col + v.dc) % nbCols)
}

case class Vector(dr:Int, dc: Int)

case class Problem(nbRows:Int, nbCols: Int, nbHeights:Int,
                   nbCells:Int, radius:Int, nbBallons:Int, nbTurns:Int,
                   startPoint: Point, targetCells: List[Point], winds: Map[Point, Vector]) {

}

