

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException
  def tail = throw new NoSuchElementException
}

object List {
  def apply[T](x1: T, x2: T) = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](x: T): List = new Cons(x, new Nil)
  def apply[T]() = new Nil
}

object values {
  val l = List(1, 2)
  val x = new Succ(Zero)
}