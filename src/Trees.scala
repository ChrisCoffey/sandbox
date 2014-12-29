
case class Tree[A](l: Option[Tree[A]], e: A, r: Option[Tree[A]])


object Tree{

  def contains[A <: PartiallyOrdered[A]](v: A, ts: Tree[A]): Boolean = {
    ts match {
      case Tree(None, e, None) if e == v => true
      case Tree(Some(l), e, _) if v < e => contains(v, l)
      case Tree(_, e, Some(r)) if e < v => contains(v, r)
      case _ => false
    }
  }

  def insert[A <: PartiallyOrdered[A]](v: A, ts: Tree[A]): Tree[A] = {
    ts match {
      case Tree(None, e, None) if e < v => Tree(Some(ts), v, None)
      case Tree(None, e, None) if v < e => Tree(None, v , Some(ts))
      case Tree(l, e, Some(r)) if e < v => Tree(l, e, Some(insert(v, r)))
      case Tree(Some(l), e, r) if v < e => Tree(Some(insert(v, l)), e, r)
      case _ => ts
    }
  }

  /**
   * Walks the Tree[A] and eventually replaces the missing node. This needs to be rebalanced,
   * but this is not a correct implementation.
   * @param v
   * @param ts
   * @tparam A
   * @return
   */
  def delete[A <: PartiallyOrdered[A]](v: A, ts: Tree[A]): Option[Tree[A]] = {
    ts match{
      case Tree(None, e, None) if e == v => None
      case Tree(l, e, Some(r)) if e < v => Some(Tree(l, e, delete(v, r)))
      case Tree(Some(l), e, r) if v < e => Some(Tree(delete(v, l), e, r))
      case _ => Some(ts)
    }
  }

  /**
   * Transforms a Tree into an ordered list of elements. The ordering is preserved by
   * walking the tree in order and appending the resultant lists together.
   * @param ts
   * @tparam A
   * @return
   */
  def toList[A <: PartiallyOrdered[A]](ts: Tree[A]): List[A] = {
    ts match{
      case Tree(None, e, None) => e :: Nil
      case Tree(Some(l), e, Some(r)) => toList(l) ::: (e :: toList(r))
      case Tree(None, e, Some(r)) => e :: toList(r)
      case Tree(Some(l), e, None) => toList(l) ::: (e :: Nil)
    }
  }
}

object TreeMap{
  type Map[K, V] = Tree[(K, V)]

  /**
   * This is a naieve compare function. In real world it'd be significantly different.
   * @param searchKey
   * @param data
   * @tparam K
   * @tparam V
   * @return
   */
  private def keyMatch[K, V](searchKey: K, data: (K, V)) = searchKey == data._1

  def lookup[K <: PartiallyOrdered, V](key: K, map: Map[K, V]): Option[V] = {
    map match{
      case Tree(l, e, r) if keyMatch(key, e) => Some(e._2)
      case Tree(Some(l), e, _) if key < e._1 => lookup(key, l)
      case Tree(_, e, Some(r)) if e._1 < key => lookup(key, r)
      case _ => None
    }
  }

  private def key[K, V](data: (K, V)) = data._1
}
