abstract class List_

case object Nil_ extends List_

case class Cons_[A](head: A, tail: List_) extends List_

object List_{
  def::[A](data: A, ls: List_) = Cons_(data, ls)

  def isEmpty(ls: List_) = ls match{
    case Nil_ => true
    case _ => false
  }

  def head[A](ls: List_) = ls match{
    case Cons_(h:A, t) => Some(h)
    case _ => None
  }

  def tail(ls: List_) = ls match{
    case Cons_(_, t: Cons_) => t
    case _ => Nil_
  }
  
  def append(as: List_, bs: List_): List_ = as match{
    case Nil_ => bs
    case Cons_(h, t) => Cons_(h, append(t,bs))
  } 

  def foldLeft[T, A](ls: List_, seed: T)(f: (A, T)=> T): T =
    ls match{
      case Nil_ => seed
      case Cons_(h: A, t) => foldLeft(t, f(h, seed))(f)
      case _ => seed
    }

  def reverse[A](ls: List_) = ls match{
    case Nil_ => ls
    case Cons_(h:A, t) => foldLeft[List_, A](ls, Nil_)((x, seed) =>{
      seed match{
        case Nil_ => Cons_[A](h, Nil_)
        case Cons_(a:A, _) => Cons_[A](a, seed)
      }
    })
  }
}


case class Queue_[A](f: List_, r: List_)

object Queue_{

  def isEmpty(q: Queue_[_]) = q match{
    case Queue_(Nil_, Nil_) => true
    case _ => false
  }

  def enQ[A](a: A, q: Queue_[A]) = Queue_(q.f, Cons_(a, q.r))

  def deQ[A](q: Queue_[A]): (Option[A], Queue_[A]) = {
    q match{
      case x if isEmpty(x) => (None, q)
      case Queue_(_Nil, x) => {
        val newF = List_.reverse(x)
        (List_.head(newF), Queue_(List_.tail(newF), Nil_))
      }
      case Queue_(f, r) =>
    }
  }

}
