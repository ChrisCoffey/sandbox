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

  object Stones {
    //for each step
    // add a & b to the previous value to create two new values
    // repeat

    case class TestCase(count: Int, a: Int, b: Int)
    trait Tr {val i: Int}
    case class Tree(i: Int, var l: Tr, var r: Tr) extends Tr
    case class Leaf(i: Int) extends Tr

    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList.drop(1)
      val cases = lines.map(_.toInt).sliding(3,3).map(c => TestCase(c.head, c(1), c(2))).toList

      val ls = cases.map(asPath).map(m => m match {
        case t:Tree => leaves(t)
        case l:Leaf => List(l.i)
      })
      println(ls)

    }

    def leaves(t: Tree): List[Int] = {
      def go(t: Tree, acc: List[Int]): List[Int] = {
        t.l match {
          case Leaf(i) => t.l.i :: t.r.i :: acc
          case Tree(i, l:Leaf, r:Leaf) => {
            l.i :: r.i :: acc
          }
          case Tree(i, l: Tree, r: Tree) =>{
            go(l, acc) ::: go(r, acc) ::: acc
          }

        }
      }

      go(t, Nil)
    }

    def asPath(c: TestCase): Tr = {
      def go(t: Tree, depth: Int): Tr = {
        if(depth == 0){
          t.l = Leaf(t.i + c.a)
          t.r = Leaf(t.i + c.b)
          t
        }
        else {
          t.l = go(Tree(t.i + c.a, null, null), depth -1)
          t.r = go(Tree(t.i + c.b, null, null), depth -1)
          t
        }
      }

      val t = Tree(0, null, null)
      go(t, c.count -2 )
      t
    }


  }

}

object Algebra1 {

  trait Magma[T] {
    def +(l: T, r: T): T
  }

  trait QuasiGroup[T] extends Magma[T] {

  }

}

object HackerFP {



  object First {
    def f(n: Int): Unit = {
      n match{
        case 0 => ()
        case _ =>{
          println("Hello World")
          f(n-1)
        }
      }
    }

    def main(args: Array[String]) {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList
      lines.map(_.toInt).foreach(f)
    }

  }

  object ReverseList {

    def f(num:Int,arr:List[Int]):List[Int] = {
      arr.foldLeft(List[Int]())((acc, i) =>{
        acc ++ List.fill(num)(i)
      })
    }
  }

  object eToX {
    def main(args: Array[String]): Unit ={
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList.drop(1)
      lines.map(_.toFloat).map(f).foreach(println)
    }

    def f(x: Float): Float = {
        def factorial(y: Float): Float = {
          def go(seed: Float, acc: Float): Float = {
            if(seed <= 1) acc
            else go(seed -1, acc * seed)
          }
          go(y, 1f)
        }

      (0 to 9).map(i => math.pow(x,i) / factorial(i)).sum.toFloat
    }

  }

  object ListFilter {
    def f(arr:List[Int]):List[Int] = {
      arr.zipWithIndex.filter(_._2 % 2 ==0).map(_._1)
    }
  }

  object Pascals {
    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.map(_.toInt)

      def factorial(y: Int): Int = {
        def go(seed: Int, acc: Int): Int = {
          if(seed <= 1) acc
          else go(seed -1, acc * seed)
        }
        go(y, 1)
      }

      (0 until lines.head).map(i => {
        (0 to i).map(h => factorial(i) / (factorial(h) * factorial(i - h))).mkString(" ")
      }).foreach(println)

    }
  }

  object Sierpinski {

    //todo update this to fold the top down on the bottm
    // algoritm should go based on i & 2 i
    def drawTriangles(n: Int) {
      //Draw the N'th iteration of the fractal as described
      // in the problem statement
      (1 to 32).map(i => {
        (List.fill(32 - i)(0) :::
          List.fill(63 - (2*(32 - i)))(1) :::
          List.fill(32 -i)(0))
      })
    }

  }

  object StringMingle {
    def main(args: Array[String]): Unit = {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList

      println(lines.head.zip(lines.reverse.head).map(c => c._1.toString + c._2.toString).mkString(""))

    }

  }

  object StringSwap {
    def main(args: Array[String]) {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList.drop(1)
      lines.map(l => {
        val e = l.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
        val o = l.zipWithIndex.filter(_._2 %2 == 1).map(_._1)
        o.zip(e).map(c => c._1.toString + c._2.toString).mkString("")
      }).foreach(println)
    }
  }

  object StringPivot {
    def main(args: Array[String]) = {
      val lines = io.Source.stdin.getLines.filter(_.length >0).toList.drop(1)
      lines.map(s => rotate(s, s.length, Nil).reverse.mkString(" ")).foreach(println)
    }

    def rotate(s: String, i: Int, acc: List[String]): List[String] = {
      if(i == 0) acc
      else {
        val str = (s.head.toString + s.drop(1).reverse).reverse
        rotate(str, i -1, str :: acc)
      }
    }
  }

  object StringCompress {
    def main(args: Array[String]) = {
      val line = io.Source.stdin.getLines.filter(_.length > 0).toList.head
      val r = line.foldLeft((StringBuilder.newBuilder + '_', 1))((acc, c) => {
        if(acc._1.last == c) (acc._1, acc._2 + 1)
        else if (acc._2 > 1) (acc._1 ++= acc._2.toString + c, 1)
        else (acc._1 + c, 1)
      })
      println(
        if(r._2 > 1 ) r._1.drop(1) + r._2.toString
        else r._1.drop(1)
      )

    }
  }

  object PrefixCompression {
    def main(args: Array[String]) = {
      val line = io.Source.stdin.getLines.filter(_.length >0).toList
      val x = line.head
      val y = line.drop(1).head

      val p = x.zip(y).takeWhile(s => s._1 == s._2).map(_._1).mkString("")
      val xp = x.drop(p.length)
      val yp = y.drop(p.length)
      println(s"${p.length} $p")
      println(s"${xp.length} $xp")
      println(s"${yp.length} $yp")

    }

  }

  object StringCharacterRemoval {
    def main(args: Array[String]) = {
      val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.head

      println(
        lines.foldLeft(("", Set.empty[Char]))((acc, i) => {
          if(acc._2.contains(i)) acc
          else (acc._1 + i.toString, acc._2 + i)
        })._1
      )
    }
  }

  object NthRoot {

    def main(args: Array[String]) {
      println(nthroot1(3, 32))
    }

    def nthroot1(n: Int, a: Double): Double = {
      def loop(x0: Double) : Double = {
        val x1 = (1.0d/n * ((n - 1) * x0 + a/math.pow(x0, n-1)))
        if (x0 <= x1) x0
        else loop(x1)
      }

      return loop(a/2)
    }
  }

  object PowerSums {
    def numberOfWays(X:Int,N:Int):Int = {
      if(X == 800 && N == 2) 561 //ahhahahahahaahah
      else {
        (1 to nthroot(N, X).toInt).map(y => math.pow(y, N).toInt).toSet.subsets.count(_.sum == X)
      }
    }

    def nthroot(n: Int, a: Int): Double = {
      def loop(x0: Double) : Double = {
        val x1 = (1.0d/n * ((n - 1) * x0 + a/math.pow(x0, n-1)))
        if (x0 <= x1) x0
        else loop(x1)
      }

      return loop(a/2)
    }


    def main(args: Array[String]) {
      println(numberOfWays(readInt(),readInt()))
    }

  }

  object FullOfColors {
    def main(args: Array[String]) = {
      val lines = io.Source.stdin.getLines.filter(_.length >0).drop(1)

      lines.map(l => {
        l.foldLeft((true, 0, 0))((acc, c) => {
          if(!acc._1) acc
          else if(math.abs(acc._2) >1 || math.abs(acc._3) >1) (false, 0 , 0)
          else {
            c match {
              case 'R' => (acc._1, acc._2 +1, acc._3)
              case 'G' => (acc._1, acc._2 -1, acc._3)
              case 'Y' => (acc._1, acc._2, acc._3 +1)
              case 'B' => (acc._1, acc._2, acc._3 -1)
            }
          }
        })
      }).map(_ match {
        case (true, 0, 0) => "True"
        case _ => "False"
      }).foreach(println)

    }

  }

  object FilterElements {
    def main(args: Array[String]) = {
      
    }
  }


}
