import scala.io.Source.stdin

@main def main(idx: Int) =
  val xs = stdin.getLines().map{_.toInt}
  print(xs.sum)

