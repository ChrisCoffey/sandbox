package sparkTest

object Main {

  def main (args: Array[String]): Unit = {
    println((0 to 1000) map (_ ^ 2))
  }
}
