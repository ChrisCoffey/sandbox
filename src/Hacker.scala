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

  object PalindromesLoveLetters {
    //algorithm is:
    // take the string
    // split it in half
    // compare each cell to the other half
    // for each compare, find the difference & accumulate it


    def main(args: Array[String]): Unit = {
      val in = io.Source.stdin.getLines.filter(_.length >0).drop(1).toList

      val pairs = in.map(l => if(l.length % 2 == 1) (l.take(l.length/2), l.drop((l.length/2) +1)) else l.splitAt(l.length/2))
      val ls = pairs.map(t => checkDistance(t._1, t._2))
      ls.foreach(println)
    }

    def checkDistance(l: String, r: String): Int = {
      (0 until l.length).foldLeft(0)((acc, i) => {
        acc + math.abs(l(i) - r(i))
      })
    }
  }

  object StickCutting {
    //given an array of integers
    //starting from the smallest
    // add the length of the list
    // and an accumulator

    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length > 0).toList
      val in = lines(1).split(" ").map(_.toInt).toList

      chopchop(in, in.min, Nil).reverse.foreach(println)
    }

    def chopchop(ls: List[Int], min: Int, acc: List[Int]): List[Int] = {
      val newMin = ls.min + min
      chopchop(ls.filter(_ > newMin), newMin, ls.length :: acc)
    }

  }

  object AngryProf {

    case class TestCase(n: Int, k: Int, students: List[Int])

    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length > 0).toList.drop(1)
      val inputs = lines.sliding(2,2).toList
      val cases = toCases(inputs)

      cases.map(c => if(classCancelled(c)) "YES" else "NO").foreach(println)

    }

    def classCancelled(c: TestCase): Boolean = {
      c.students.count(_ > 0) < c.k
    }

    def toCases(in: List[List[String]]): List[TestCase] = {
      in.map(p => {
        val l1 =  p.head.split(" ").map(_.toInt).toList
        val l2 = p(1).split(" ").map(_.toInt).toList
        TestCase(l1.head, l1(1), l2)
      })
    }

  }

  object HalloweenParty {

    // 1 2 3 4 5 6 7  8  9  10
    // 0 1 2 4 6 9 12 16 20 25

    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList.drop(1)
      lines.map(_.toLong).map(i => math.floor((i * i)/4)).foreach(println)
    }

  }

  object DivisibleDigits {

    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList.drop(1)

      val pairs = lines.map(l => (l.toInt, l.toList.map(_ - 48).filter(_ > 0)))
      val rs = pairs.map(p => p._2.filter(i => p._1 % i == 0).length)
      rs.foreach(println)
    }

  }
}
