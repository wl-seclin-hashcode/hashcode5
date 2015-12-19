package hashcode

object Parser {
  def read(): Problem = {
    val input = io.Source.fromFile("input/data.in").getLines

    val Array(nbRows, nbCols, nbHeights) = input.next().split(" ").map(_.toInt)
    val Array(nbCells, radius, nbBallons, nbTurns) = input.next().split(" ").map(_.toInt)
    val Array(startRow, startCol) = input.next().split(" ").map(_.toInt)

    val targetCells = Vector.fill(nbCells) { input.next().split(" ").map(_.toInt) }
    val sections = Vector.fill(nbHeights) {
      Vector.fill(nbRows) {
        val winds = input.next().split(" ").map(_.toInt).grouped(2)
        winds.map({ case Array(row, col) => WindVector(row, col) }).toList
      }
    }

    Problem(nbRows, nbCols, nbHeights, nbCells, radius, nbBallons, nbTurns, Point(startRow, startCol, 0),
      targetCells.toVector.map { case Array(x, y) => Point(x, y, 0) },
      sections.zipWithIndex.flatMap {
        case (list, height) => list.zipWithIndex.map {
          case (line, row) => line.zipWithIndex.map {
            case (wv, col) => Point(row, col, height + 1) -> wv
          }
        }
      }.flatten.toMap)

  }
}