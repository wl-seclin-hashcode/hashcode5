package hashcode

object Parser {
  def read(): Problem = {
    val input = io.Source.fromFile("input/data.in").getLines.toList
    val first = input.head
    val Array(nbRows, nbSlots, nbUnavailable, nbPools, nbServers) = first.split(" ").map(_.toInt)

    val servers = for {
      (line, i) <- input.tail.drop(nbUnavailable).take(nbServers).zipWithIndex
      Array(s, c) = line.split(" ")
    } yield Server(s.toInt, c.toInt, i)

    Problem(servers)
  }
}