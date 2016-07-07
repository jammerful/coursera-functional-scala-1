package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r ==0 || c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty || acc < 0 ) acc
      else if(chars.head == '(') balanceIter(chars.tail, acc + 1)
      else if(chars.head == ')') balanceIter(chars.tail, acc - 1)
      else balanceIter(chars.tail, acc)
    }
    balanceIter(chars, 0) == 0
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

}
