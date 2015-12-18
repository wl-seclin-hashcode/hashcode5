package hashcode

case class Point(row:Int, col:Int, height: Int) {
  def addHeight(h: Int) = this.copy(height = height+h)   // TODO OVERFLOW !
  def addVector(v:Vector) = this.copy(row= row + v.dr, col = (col + v.dc) % 300)
}

case class Vector(dr:Int, dc: Int)

case class Problem(nbRows:Int, nbCols: Int, nbHeights:Int,
                   nbCells:Int, radius:Int, nbBallons:Int, nbTurns:Int,
                   startPoint: Point, targetCells: List[Point], winds: Map[Point, Vector]) {

}

