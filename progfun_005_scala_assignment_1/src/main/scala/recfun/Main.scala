package recfun
import common._

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
   * e.g: 
   * pascal(0,2) = 1
   * pascal(1,2) = 2
   * pascal(1,3) = 3
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || r == c) 
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(status: Int, chars: List[Char]): Boolean = {        
      if (chars.isEmpty)
        status == 0
      else if (chars.head == '(')
        balanceHelper(status+1, chars.tail)
      else if (chars.head == ')')
        status >= 1 && balanceHelper(status-1, chars.tail)
      else  
        balanceHelper(status, chars.tail)
    }
    balanceHelper(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else 
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
