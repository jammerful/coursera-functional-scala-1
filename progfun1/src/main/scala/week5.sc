import math.Ordering

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("Last of an empty list!")
  case List(z) => z
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("Init of an empty list!")
  case List(z) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs,ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ::: List(y)
}

def remove[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop n+1)

def flatten[Any](xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case y :: ys => (y match {
    case l: List[Any] => flatten(l)
    case i => List(i)
  }) ::: flatten(ys)
}

val x = List(1, 2, 3, 4, 5)
val y = List(6, 7, 8, 9)
println(last(x))
println(init(x))
println(concat(x,y))
println(reverse(x))
println(remove(x,0))
println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

/// Pairs, Tuples, and Implicit Paramaters

def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
  case List() => ys
  case x :: xs1 => ys match {
    case List() => x :: xs1
    case y :: ys1 =>
      if (x < y) x :: y :: merge(xs1, ys1)
      else y :: x :: merge(xs1, ys1)
  }
}

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (List(),List()) => List()
      case (List(), z :: zs) => ys
      case (w :: ws, List()) => ws
      case (a :: as, b :: bs) =>
        if (ord.lt(a,b)) a :: b :: merge(as, bs)
        else b :: a :: merge(as, bs)
    }
    val (first, second) = xs splitAt n
    merge(msort(first), msort(second))
  }
}

val a = List(3,4,1,2,9,5,12)
println(msort(a))

val pair = ("answer", 42)


//// Higher Order List Functions

//abstract class List[T] {
//  def map[U](f: T => U): List[U] = this match {
//    case List() => List()
//    case y :: ys => f(y) :: ys.map(f)
//  }
//
//  def filter(p: T => Boolean): List[T] = this match {
//    case List() => this
//    case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
//  }
//}

def pack1[T](xs: List[T]): List[List[T]] = {
  def loop(list: List[T], acc: List[List[T]]): List[List[T]] = list match {
    case Nil => acc
    case y :: ys =>
      if (ys.contains(y)) loop(ys.filterNot(_ == y), acc :+ List.fill(ys.count(_ == y) + 1)(y))
      else loop(ys.filterNot(_ == y ), acc :+ List(y))
  }
  loop(xs, List())
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T,Int)] = pack(xs).map(ys => (ys.head, ys.length))


println(pack1(List(1,2,1,3,3,4)))
println(pack(List(1,2,1,3,3,4)))
println(encode(List("a", "a", "a", "b", "c", "c", "a")))

///// List Reduction: fold

def sum1(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum1(ys)
}

def sum2(xs: List[Int]): Int = (0 :: xs) reduceLeft(_ + _)

def sum3(xs: List[Int]): Int = (xs foldLeft 0 )(_ + _)


println(sum1(a))
println(sum2(a))
println(sum3(a))

def concat2[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _)

println(concat2(x,y))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldLeft 0)((n,_) => n+1)
//(xs foldLeft 0)((_,n) => n+1)

println(mapFun(x, (y: Int) => y+1))
println(lengthFun(x))