package hashcode

case class Server(size: Int, capacity: Int, id: Int) {
  def ratio: Float = capacity / size.toFloat
}

case class Point(row:Int, col:Int, height: Int) {

}

case class Vector(dr:Int, dc: Int)

case class Problem(nbRows:Int, nbCols: Int, nbHeights:Int,
                   nbCells:Int, radius:Int, nbBallons:Int, nbTurns:Int,
                   startPoint: Point, targetCell: List[Point], winds: Map[Point, Vector]) {

}

