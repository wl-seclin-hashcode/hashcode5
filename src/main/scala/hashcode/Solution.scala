package hashcode

case class Command(balloonId: Int, move: Int, round: Int)
case class Solution(sol: Vector[Command]) {
  val byRound = sol.groupBy(_.round)
  val byBalloon = sol.groupBy(_.balloonId)
}