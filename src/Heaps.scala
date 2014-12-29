
case class LeftistHeap[T](rank: Int, elem: T, l: LeftistHeap[T], r: LeftistHeap[T])

object LeftistHeap{
  def merge[T <: PartiallyOrdered[T]](a: Option[LeftistHeap[T]], b: Option[LeftistHeap[T]])
  : Option[LeftistHeap[T]] = {
    (a, b) match{
      case (None, Some(h)) => Some(h)
      case (Some(h), None) => Some(h)
      case (Some(LeftistHeap(_, x, l1, r1)), Some(LeftistHeap(_, y, l2, r2))) =>{
        if (x < y) makeHeap(x, l1, merge(Some(r1), b))
      }
    }
  }

  def makeHeap[T](e: T, a: LeftistHeap[T], b: LeftistHeap[T]) = {

  }

  def rank(h: Option[LeftistHeap[_]]) = h.map(_.rank).getOrElse(0)
}