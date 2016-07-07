object fold {
  val list = List(1,2,3)
  list.foldLeft(0)(_ + _)

  def fact(n: Int): Int = (1 to n).foldRight(1)(_ * _)
  fact(3)

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  isort(List(3,1,2))

  val y = 1 :: 2 :: 3 :: Nil

}