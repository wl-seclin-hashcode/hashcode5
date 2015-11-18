package hashcode

case class Server(size: Int, capacity: Int, id: Int) {
  def ratio: Float = capacity / size.toFloat
}

case class Problem(servers: List[Server])

