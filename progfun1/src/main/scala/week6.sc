val nums = Vector(1,2,4,-3)

val xs = Array(3,1,6,4)

xs map (x => x+1)

val s = "Hello world"

s filter (c => c.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1,2,3) zip s
pairs.unzip

s flatMap (c => List('.',c))

xs.sum
xs.max
xs.min

def isPrime(n: Int): Boolean = (2 until n) forall (d => (n % d != 0))

isPrime(23)
isPrime(22)

////

val n: Int = 7
(1 to n) flatMap (i => (1 until i) map (j => (i,j))) filter(pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if (isPrime(i+j))
} yield (i,j)

def scalarProduct(xs: List[Int], ys: List[Int]): Int =
  (for ((x,y) <- xs zip ys) yield x*y).sum

val x = List(1,2,3)
val y = List(1,2,3)

scalarProduct(x,y)

///// n-queens

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => (col != c && math.abs(col - c) != row - r)
  }
}

def nqueens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if(k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k-1)
        col <- 1 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  placeQueens(n)
}

println(nqueens(4))

///// maps

class Poly(terms0: Map[Int,Double]) {
  def this(bindings: (Int,Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  //def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def + (other: Poly) = new Poly(other.terms foldRight terms (addterms))
//  private def adjust(term: (Int, Double)): (Int, Double) = {
//    val (exp, coeff) = term
//    exp -> (coeff + terms(exp))
//  }
  private def addterms(terms: Map[Int,Double], term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }
  override def toString() =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+ "x^"+exp) mkString " + "
}

val p1 = new Poly(0 -> 5, 2 -> 4, 3 -> 2)