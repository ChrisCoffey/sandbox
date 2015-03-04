def map(i: Int)(f: Int => String): String

def map2(a: Int, b: Int)(f: (Int, Int) => String): String


trait Gen[A] {

 val count: Int
 val f: () => A
 private def transform(i: int): A = f()
 val cases: Stream[A] =  Stream.from(1).take(count).map(transform) 

}

object Gen {

}

object Prop {

  def &&(l: Prop, r: Prop): Prop = new Prop{
 		def apply(a: A) = if (l.apply(a)) r.apply(a) else false 
  }

  def forAll(p: Prop)(g: Gen[A]):  = g.cases.map(a => (prop.apply(a), a))

}



trait Prop{
 type SuccessCount = Int
 type Message = String

 def apply(a: A): Boolean
 def check[A](g: Gen[A): Either[Message, SuccessCount]
}
