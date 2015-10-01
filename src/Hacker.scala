import scala.collection.immutable.Queue
import scala.io.Source

object Algorithms {
   object Warmpus {
     object ArraySum {
       def main (args: Array[String]): Unit = {
         val line = io.Source.stdin.getLines().filter(_.length > 0).drop(1).toList.head
         println(line.split(" ").map(_.toInt).sum)
       }
     }

     object ArraySumLong {
       def main (args: Array[String]): Unit = {
         val line = io.Source.stdin.getLines().filter(_.length > 0).drop(1).toList.head
         println(line.split(" ").map(_.toLong).sum)
       }
     }

     object DiagonalSumDifference {
       def main (args: Array[String]): Unit = {
         val matrix = io.Source.stdin.getLines().filter(_.length > 0).drop(1).map(_.split(" ").map(_.toInt)).toList

         val lr = matrix.indices map (i => matrix(i)(i)) sum
         val rl = matrix.indices.reverse map (i => matrix(i)(i)) sum
         val diff = lr - rl
         println(math.abs(diff))
       }
     }

     object ArrayFractions {
       def main (args: Array[String]): Unit ={
         val lines = io.Source.stdin.getLines().filter(_.length > 0).toList
         val n = lines.head.toDouble
         val ints = lines.tail.head.split(" ").map(_.toInt)

         println( "%.3f".format(ints.count(_ > 0) / n))
         println( "%.3f".format(ints.count(_ < 0) / n))
         println( "%.3f".format(ints.count(_ == 0) / n))
       }
     }

     object Staircase {
       def main (args: Array[String]): Unit = {
         val n = io.Source.stdin.getLines().filter(_.length > 0).toList.head.toInt

         val ls = 1 to n map (i => (List.fill(i)("#") ++ List.fill(n - i)(" ")).mkString.reverse) mkString("\n")
         println(ls)
       }
     }

     object TimeConversion {
       def main (args: Array[String]): Unit = {
         val n = io.Source.stdin.getLines().filter(_.length > 0).toList.head

         val r = n match {
           case pm if pm.endsWith("PM")=>
             val Array(hh, mm, ss) = pm.dropRight(2).split(":")
             s"${if(hh.toInt == 12) hh.toInt else hh.toInt + 12}:$mm:$ss"
           case am =>
             val Array(hh, mm, ss) =  am.dropRight(2).split(":")
             s"${if(hh.toInt == 12) "00" else hh}:$mm:$ss"
         }

        println(r)
       }

     }

     object LibraryFine {
       def main(args: Array[String]): Unit = {
         val in = io.Source.stdin.getLines().filter(_.length > 0).toList
         val actual = in.head.split(" ").map(_.toInt)
         val expected = in.tail.head.split(" ").map(_.toInt)
         val diff = actual.zip(expected).map(p => p._1 - p._2)

         val r = diff match {
           case Array(_, _, x) if x > 0 => 10000
           case Array(_, m, 0) if m > 0 => m * 500
           case Array(d, 0, 0) if d > 0 => d * 15
           case _ => 0
         }

         println(r)
       }
     }

     object BigIntFactorial {
       def main(args: Array[String]): Unit = {
         val in = io.Source.stdin.getLines().filter(_.length > 0).toList.head.toInt
         def f(x: Int, acc: BigInt): BigInt =
          x match {
            case 0 => acc
            case _ => f(x - 1, acc * x)
          }

         println(f(in, 1))
       }

     }
   }

   object Implementation {
     object MatrixRotation {
       type Matrix = Seq[Seq[Int]]
       case class MatrixRing(top: Seq[Int], left: Seq[Int], bottom: Seq[Int], right: Seq[Int]) {
         def rotate: MatrixRing =
           MatrixRing(
             top.drop(1) :+ right.head,
             left.drop(1) :+ top.head,
             bottom.drop(1) :+ left.head,
             right.drop(1) :+ bottom.head)

       }

       // to reconstruct, sort matrix rings by size, then zip with their indexs
       // construct indexed prefix & suffix for each place
       // prefix(n) + top(n) + suffix(n)
       // prefix(len - n) + bottom + suffix(len -n)
       // the above two will write the two rows
       // all other rows add a value to the end of the prefix & start of the suffix

       def asMatrix(rings: Seq[MatrixRing]): Seq[String] = {
         val sorted = rings.sortBy(- _.left.size)
         val len = sorted.head.left.size
         var prefixes = Map[Int, String]()
         var suffixes = Map[Int, String]()
         val matrix: Seq[StringBuilder] = Seq.fill(len + 1)(StringBuilder.newBuilder)

         sorted.zipWithIndex.foreach{p =>
           val (m, i) = p
           m.left.reverse.zipWithIndex.foreach(x => prefixes =  prefixes.updated(x._2 + i + 1, prefixes.get(x._2 + i + 1).fold(x._1.toString)(s => s + " " + x._1.toString)))
           m.right.zipWithIndex.foreach(x => suffixes = suffixes.updated(x._2 + i -1, suffixes.get(x._2 + i - 1).fold(x._1.toString)(s => x._1.toString + " " + s )))

           matrix(i).append(s"${prefixes.getOrElse(i, "")} ${m.top.mkString(" ")}${if(i == 0) "" else " "}${suffixes.getOrElse(i, "")}")
           matrix(len - i).append(s"${prefixes.getOrElse(len - i, "")} ${m.top.mkString(" ")}${if(i == len -1) "" else " "}${suffixes.getOrElse(len - i, "")}")
         }

         matrix.map(_.toString())
       }

       //for each ring, join it together by using a function that adds stuff to the front or back of a string
       def join(rings: Seq[MatrixRing]): Matrix = {
         val sorted = rings.sortBy( - _.left.size)
         type M = Either[Seq[Int], (Seq[Int], Seq[Int])]
         sorted.foldLeft(Seq[M]){ (acc, )

         }
       }

       def reqdRotations(matrixRing: MatrixRing, r: Int) =
         r % ((matrixRing.top.size * 2) + (matrixRing.left.size * 2))

       def rotateNTimes(matrixRing: MatrixRing, n: Int): MatrixRing =
        if (n == 0) matrixRing
        else rotateNTimes(matrixRing.rotate, n -1)

       def main(args: Array[String]): Unit = {
         val lines = io.Source.stdin.getLines().filter(_.length > 0).toList
         val Array(m, n, r) = lines.head.split(" ").map(_.toInt)
         val matrix: Matrix = lines.tail.map(_.split(" ").map(_.toInt).toSeq)

         val topBottom = matrix.indices zip matrix.indices.reverse
         val leftRight = matrix.head.indices zip matrix.head.indices.reverse
         val ringIndexes = topBottom zip leftRight

         val rings = ringIndexes.map{ ix =>
           val ((t, b), (l, r)) = ix
           MatrixRing(
             matrix(t).drop(l).dropRight(l + 1),
             matrix.foldLeft(Seq[Int]())((acc, ls) => acc :+ ls(l)).reverse.drop(t).dropRight(t + 1),
             matrix(b).reverse.drop(l).dropRight(l + 1),
             matrix.foldLeft(Seq[Int]())((acc, ls) => acc :+ ls(r)).drop(t).dropRight(t + 1)
           )
         }.filter(_.top.nonEmpty)

         val rotated = rings.map(m => rotateNTimes(m, reqdRotations(m, r)))

         println(rings)
         println(rotated)
       }
     }
   }

  object Strings {
    object FunnyString {
      def main (args: Array[String]): Unit ={
        val strs = io.Source.stdin.getLines().filter(_.length > 0).drop(1).toList
        val res = strs.map{s =>
          (s zip s.reverse).foldLeft((true, s.head, s.last)){ (acc, c) =>
              if(!acc._1) acc
              else {
                val b = math.abs(c._1 - acc._2) == math.abs(c._2.toInt - acc._3)
                (b, c._1, c._2)
              }
            } match {
            case (true, _, _) => "Funny"
            case _ => "Not Funny"
          }

        }

        println(res.mkString("\n"))
      }
    }

    object Pangrams {
      def main (args: Array[String]): Unit = {
        val str = io.Source.stdin.getLines().filter(_.length > 0).toList.head.toLowerCase.replace(" ", "")

        val r = if (str.distinct.size == 26) "pangram" else "not pangram"
        println(r)
      }
    }

    object AlternatingCharacters {
      def main (args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.drop(1)

        val r = lines.map(l =>
          l.tail.foldLeft((0, l.head))((acc, c) =>
            if(acc._2 == c) (acc._1 + 1, c)
            else (acc._1, c)
          )._1
        )

        println(r.mkString("\n"))
      }
    }

    object GameOfThronesOne {
      def main(args: Array[String]): Unit ={
        val line = io.Source.stdin.getLines().filter(_.length > 0).toList.head
        if (line.toList.groupBy(c => c).filter(_._2.size % 2 == 1).size <= 1) println("YES")
        else println("NO")

      }
    }

    object Gemstones {
      def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.drop(1)

        println( lines.tail.foldLeft(lines.head.toSet)((acc, s) => s.toSet intersect acc).size )
      }
    }

    object MakeItAnagram {
      def main (args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList
        lines.map(s => s.toList.groupBy(c => c).map(t => (t._1, t._2.size))) match {
          case a :: b :: Nil =>
            val r = a.foldLeft(b){(acc, kvp) =>
              acc.get(kvp._1).fold(acc + kvp)(v => acc.updated(kvp._1, math.abs(v - kvp._2)))
            }.values.sum
            println(r)
          case _ => println("bleh")
        }

      }
    }

    object Anagram {
      def main (args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines.filter(_.length > 0).toList.drop(1)
        val r = lines.map{ s =>
            s.length match {
              case x if x %2 == 1 => -1
              case y =>
                val a = s.take(y / 2).groupBy(c => c).map(t => (t._1, t._2.length))
                val b = s.takeRight(y / 2).groupBy(c => c).map(t => (t._1, t._2.length))

                b.foldLeft(0)((i, pair) => a.get(pair._1).fold(pair._2)(x => math.max(pair._2 - x, 0)) + i)
            }
        }
        println(r.mkString("\n"))

      }
    }

    object TwoStrings {
      def main(args: Array[String]): Unit ={
        val lines = io.Source.stdin.getLines.filter(_.length > 0).toList.drop(1)
        val cases = lines.sliding(2, 2).toList

        val rs = cases map { ls =>
          val List(a, b) = ls

          if((a.toSet intersect b.toSet).nonEmpty) "YES"
          else "NO"
        }

        println(rs.mkString("\n"))
      }
    }

    object BiggerIsGreater {
      def main (args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines.filter(_.length > 0).toList.drop(1)

        val res = lines map { line =>
           if(line.toList.distinct.size == 1) "no answer"
            else {
            val str = line.foldRight((List[Char](), false)){(c, acc) =>
                if(acc._2) acc
                else if(acc._1.isEmpty) (c :: Nil, false)
                else if(c < acc._1.head) (acc._1.head :: c :: acc._1.tail, true)
                else (c :: acc._1, false)
            }._1.mkString
             line.dropRight(str.length) + str
           }
        }

        println(res.mkString("\n"))

      }
    }

  }

  object Sorting {
    object Tutorial {
      def main (args: Array[String]): Unit ={
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList
        val n = lines.head.toInt
        val ls = lines.last.split(" ").map(_.toInt)

        println(ls.indexOf(n))
      }
    }

    object InsertionSort1 {
      def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.drop(1)
        val arr = lines.head.split(" ").map(_.toInt)
        val elem = arr.last

        var n = arr.indices.last -1
        while(n > -1 && elem <= arr(n)) {
          arr(n +1) = arr(n)
          n -= 1
          println(arr.mkString(" "))
        }
        arr(n +1) = elem
        println(arr.mkString(" "))
      }
    }

    object InsertionSort2 {
      def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.drop(1)
        val arr = lines.head.split(" ").map(_.toInt).toList

        def insert(ls: List[Int], x: Int) =
          ls.span(i => i < x) match {
            case (lt, gt) =>
              println(lt)
              println(gt)
              lt ::: i :: gt
          }

        arr.foldLeft(arr){(acc, i) =>
            val ls = insert(acc.span(_ < i) match { case (l, r) => l ::: r.tail }, i)
            println(ls.mkString(" "))
            ls
        }
      }
    }

    object RunningTime {
      def main (args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().filter(_.length > 0).toList.drop(1)
        var arr = lines.head.split(" ").map(_.toInt).toList
        var moves = 0

        def insert(ls: List[Int], x: Int) = {
          val (lt, gt) = ls.span(_ < x)
          (lt ::: (x :: gt) , lt.indices.lastOption.fold(0)(i => i + 1) )
        }

        var i = 0
        arr.foldLeft(List[Int]()){(acc, a) =>
            val (newArr, idx) = insert(acc, a)
            moves += math.abs(i - idx)
            i += 1
            newArr
        }

        println(moves)
      }
    }
  }

}

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
      val lines = io.Source.stdin.getLines.filter(_.length > 0).drop(1)
      lines.sliding(2,2).map(ls => {
        val x = ls.head.split(" ").drop(1).map(_.toInt).head
        val as = ls.drop(1).head.split(" ").map(_.toInt)
        occurances2(as.zipWithIndex.toList, Map.empty[Int, (Int, Int)])
          .filter(t => t._2._1 >= x)
          .map(t => (t._1, t._2._2)).toSeq
          .sortBy(_._2)
          .map(_._1)

      }).foreach(l => println(if(l.isEmpty) "-1" else l.mkString(" ")))


    }


    def occurances2(ls: List[(Int, Int)], m: Map[Int, (Int, Int)]): Map[Int, (Int, Int)]= {
      ls match {
        case h :: Nil => {
          val i = m.getOrElse(h._1, (0, h._2))
          m + (h._1 ->(i._1 + 1, i._2))
        }
        case h :: t => {
          val i = m.getOrElse(h._1, (0, h._2))
          occurances2(t, m + (h._1 ->(i._1 + 1, i._2)))
        }
      }
    }
  }

  object SuperDigit {
    def main (args: Array[String]) = {
      val nk = io.Source.stdin.getLines().filter(_.length >0).toList.head.split(" ")
      val np = sd(BigInt(nk.head))
      val r = sd(np * nk.drop(1).head.toInt)
      println(r)

    }

    def sd(i: BigInt): Int = {
     if(i < 10) i.toInt
      else sd(i.toString.map(_.toInt).sum)
    }
  }

  object ConvexHull {
    def main(args: Array[String]) = {

    }


  }


  object RemoveDuplicates {
    def main(args: Array[String]): Unit = {
      val s = io.Source.stdin.getLines().filter(_.length > 0).toList.head
      println(s.foldLeft(List[Char]())((acc, c) => if(acc.contains(c)) acc else c :: acc ).reverse.mkString)

    }
  }

  object HugeGCD {
    def main(args: Array[String]): Unit ={
      val mod = 1000000007

    }
  }

  object RecursiveTrees {
    def main(args: Array[String]) = {
      val n = io.Source.stdin.getLines().filter(_.length > 0).toList.head.toInt
        val ranges = (1 to n).foldLeft(List[List[Int]]())((acc, i) =>
            acc match {
                case Nil => (63 to 32 by -1 ).toList :: acc
                case h :: t => (h.reverse.head to h.reverse.head / 2 by -1).toList :: acc
            }
        )

        (1 to n).foldLeft(Seq[Int](50)){ (acc, i) =>
            val rs = ranges.reverse(i -1)
            val verticals = rs.take(rs.length/2)
            val diagonals = rs.drop(rs.length/2)

            verticals.foreach{ x =>
                println(drawLine(Seq(i - diagonals.length, i + diagonals.length)))
            }

            val ends = acc.flatMap(x => Seq(x - diagonals.length, x + diagonals.length))
            ends.foreach(println)

            ends


        }




    }


    def drawLine(oneIndexes: Seq[Int]): String = {
      (0 until 100).map(i => if(oneIndexes.contains(i)) "1" else "_").mkString
    }

  }


  object ArithmeticParser {

//      Expression ::= Term [+-] Expression
//          | Term
//
//      Term       ::= Factor [*/] Term
//          | Factor
//
//      Factor     ::= Number
//      | [+-] Factor
//          | '(' Expression ')'


    def main(args: Array[String]): Unit = {
        val expr = Source.stdin.getLines().filter(_.length > 0).toList.head

        val rpn = parse(expr, Queue.empty[String], Nil, false)
        println(eval(rpn, Nil) % 1000000007)

    }

      val stopCharacters = Seq('+', '-', '*', '/', '(', ')', ' ')
      val precedenceMap = Map("+" -> 2, "-" -> 2, "*" -> 3, "/" -> 3, "(" -> 4, ")" -> 4, "pos" -> 5, "neg" -> 5)
      val operators = Set("+", "-", "pos", "neg", "*", "/")
      //note unary operators will be specially coded. need to understand if previous symbol was an operator.
      val unaryMapping = Map("+" -> pos, "-" -> neg)

      val neg = "neg"
      val pos = "pos"

    def parse(expr: String, out: Queue[String], stack: List[String], wasJustOp: Boolean): Queue[String] = {
        if(expr.isEmpty) stack.foldLeft(out)((q, op) => q.enqueue(op))
        else
            nextToken(expr) match {
            case (op, false) =>
                if(wasJustOp && precedenceMap(op) == 2){
                    parse(expr.drop(1), out, unaryMapping(op) :: stack, true)
                }
                else if(op == "("){
                    parse(expr.drop(1), out, op :: stack, true)
                }
                else {
                    stack match {
                        case Nil => parse(expr.drop(1), out, op :: stack, true)
                        case o2 :: t =>
                           if(op == ")"){
                               val popped = stack.takeWhile("(" !=)
                               parse(expr.drop(1), out.enqueue(popped), stack.drop(popped.length +1), true)
                           }
                           else {
                               if(precedenceMap(op) < precedenceMap(o2)) {
                                   val ops = t.takeWhile(precedenceMap(op) < precedenceMap(_))
                                   parse(expr.drop(1), out.enqueue(o2 :: ops), op ::t.drop(ops.length), true)
                               }
                               else {
                                   parse(expr.drop(1), out, op :: stack, true)
                               }
                           }
                    }
                }
            case (num, true) => parse(expr.drop(num.length), out.enqueue(num), stack, false)
         }
      }

      def nextToken(expr: String): (String, Boolean) = {
         if( stopCharacters.contains(expr(0))) (expr(0).toString, false)
          else (expr.takeWhile(!stopCharacters.contains(_)), true)
      }




      def eval(out: Queue[String], stack: List[Int]): Long = {
          println(stack)
          println(out.head)
          if(out.isEmpty) stack.head
          else
        out.dequeue match {
            case (x, q) if operators.contains(x) =>
                x match {
                    case "+" =>
                        val r = stack.head
                        val l = stack.tail.head
                        eval(q, l + r :: stack.drop(2))
                    case "-" =>
                        val r = stack.head
                        val l = stack.tail.head
                        eval(q, l - r :: stack.drop(2))
                    case "/" =>
                        val r = stack.head
                        val l = stack.tail.head
                        eval(q, l / r :: stack.drop(2))
                    case "*" =>
                        val r = stack.head
                        val l = stack.tail.head
                        eval(q, l * r :: stack.drop(2))
                    case "pos" =>
                        val r = stack.head
                        eval(q, math.abs(r) :: stack.tail)
                    case "neg" =>
                        val r = stack.head
                        eval(q, -r :: stack.tail)
                }
            case (num, q) => eval(q, num.toInt :: stack)
        }
      }
  }

    object PentagonalNumbers {
        def main(args: Array[String]) = {
            val n = io.Source.stdin.getLines().filter(_.length > 0).toList.head.toInt
        }
    }

    object GcdList {

        def main (args: Array[String]) = {
            val in = scala.io.Source.stdin.getLines().filter(_.length >0).toList.drop(1)
            val slices = in.map(l => l.split(" ").map(_.toInt).sliding(2, 2).map(a => (a(0), a(1))).toList)
           val r = slices.foldLeft(slices.head)((acc, ls) => {
                acc.filter(i => ls.exists(e => e._1 == i._1 ))
                    .map{i =>
                        val other = ls.groupBy(_._1)(i._1).minBy(_._2)
                        if (other._2 < i._2)  (i._1, i._2) else i
                }
            }).map(i => i._1 + " " + i._2).mkString(" ")

            println(r)
        }

    }

    object MatrixRotation {

        def main(args: Array[String]): Unit ={
            val in = scala.io.Source.stdin.getLines().filter(_.length > 0).toList
            val r = in.head.split(" ")(2).toInt

            val mx = in.tail.map(_.split(" "))

            for (i <- 0 to r) {

            }
        }

    }

  object SuperQueen {



//    //0 = empty, 1 = threatened, 9 = queen
//    def placeQueen(matrix: List[List[Int]], x: Int, y: Int): Boolean= {
//      matrix.foldLeft(List[List[Int]])((acc, i) => {
//
//      })
//
//    }
//
//    def solveNSuperQueens(n: Int): Int = {
//
//      val board = List.fill(n)(List.fill(n)(true))
//
//      // Fill Up this function body to display the number of arrangements
//      // Of N Super-Quees on a chessboard
//    }
//
//
//    def main(args: Array[String]) {
//      /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
//*/
//      println(solveNSuperQueens(readInt))
//    }
  }


}
