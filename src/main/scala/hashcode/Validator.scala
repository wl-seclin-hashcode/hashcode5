package hashcode

import scala.annotation.tailrec
import scala.util.Success
import scala.util.Try

object Validator {
  def score(solution: Solution, problem: Problem): Try[Int] =
    Success(solution.dummy)
}