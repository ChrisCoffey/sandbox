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

  object ChocolateFeast {


    case class TestCase(n: Int, c: Int, m: Int)

    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length > 0).toList.drop(1)
      val cases = lines.map(toCases)
      cases.map(howMuchCandy).map(println)
    }

    def howMuchCandy(c: TestCase): Int = {
      val bought = c.n / c.c
      val exchanged = bought / c.m
      val wrappers = (bought % c.m) + exchanged

      println(wrappers)
      def leftovers(w: Int, acc: Int): Int =
        w / c.m match {
          case 0 => acc
          case x => {
            leftovers((x % c.m) + (w/c.m), acc + (w/c.m))
          }
        }
      val allLeftovers = leftovers(wrappers, 0)

      bought + exchanged + allLeftovers
    }

    def toCases(s: String): TestCase = {
      val ls = s.split(" ").map(_.toInt)
      TestCase(ls.head, ls(1), ls(2))
    }
  }

  object Cavities {

    case class TestCase(ls: List[List[Int]])
    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length > 0).toList.drop(1)
      val c = TestCase(lines.map(_.map(c => c.toInt - 48).toList))

      val h = c.ls.map(adjacent)
      val v = rotate(rotate(c.ls).map(adjacent))

      merge(h, v).map(ls =>
        ls.map(p =>
          if(p._2) "X" else p._1.toString
        ).mkString(""))
        .foreach(println)

    }

    def adjacent(ls: List[Int]): List[(Int, Boolean)] = {
      ls.zipWithIndex.map(l => {
        val x = l._1
        if(l._2 ==0 || l._2 == ls.length -1) (x, false)
        else (x, x > ls(l._2-1) && x > ls(l._2 + 1))
      })
    }

    //rotate the matrix
    def rotate[T](tc: List[List[T]]): List[List[T]] = {
      (0 until tc.head.length).foldLeft(List[List[T]]())((acc, i) => {
        tc.map(_(i)) :: acc
      }).reverse
    }

    def merge(a: List[List[(Int, Boolean)]], b: List[List[(Int, Boolean)]]): List[List[(Int, Boolean)]] = {
      (0 until a.length).foldLeft(List[List[(Int, Boolean)]]())((acc, i) => {
        a(i).zipWithIndex.map(p => (p._1._1, b(i)(p._2)._2 && p._1._2)) :: acc
      }).reverse
    }

  }

  

}
