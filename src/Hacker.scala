object Hacker {

  object Helpers{
    val space = " "
  }

  object UtopianTree extends App {
    val lines = io.Source.stdin.getLines().filter(_.length > 0).drop(1)
    val nums = lines.map(_.toInt)
    nums.map(grow).foreach(println)

    def grow(cycles: Int): Int =
      (1 to cycles).foldLeft(1)((acc, i) => {
        if(i %2 == 1) acc + acc else (acc + 1)
      })

  }

  object MaxXor {

    def maxXor(l: Int, r: Int): Int = {

      val rng = l to r
      rng.foldLeft(0)((acc, i) => {
        rng.filter(_ > i).map(i ^ _).max
      })

    }

    def main(args: Array[String]) {
      var _l:Int = Console.readInt


      var _r:Int = Console.readInt


      val res =         maxXor(_l, _r)
      println(res)


    }
  }

  object HighwayRange {

    def main(args: Array[String]) {
      val lines = io.Source.stdin.getLines().filter(_.length > 0).toList
      val len = lines.head.split(Helpers.space).head.map(_.toInt)
      val ws = lines(1).split(Helpers.space).map(_.toInt)
      val cases = lines.drop(2).map(_.split(Helpers.space).map(_.toInt))

      println(cases.map(a => path(ws)(a(0), a(1)).min))

    }

    def path(path: Seq[Int])(s: Int, e: Int): Seq[Int] =
      path.drop(s).take(math.max(e - s + 1, 1))
  }

  object LonelyInt {
    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length > 0).toList
      val in = lines(1).split(" ").map(_.toInt)

      println(in.reduce(_ ^ _))
    }
  }


}
