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

    println()
    println("Parentheses Balancing")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println( balance(":-)".toList))
    println(balance("())(".toList))

    println()
    println("Counting change")
    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
   if(c == 0 || c == r ) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanced(find: Int, chars: List[Char]): Boolean = {
      if(chars.isEmpty) find == 0
      else
        if (chars.head == '(') balanced(find+1, chars.tail)
        else
          if (chars.head == ')') find > 0 && balanced(find-1, chars.tail)
          else balanced(find, chars.tail)
    }
    balanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(capacity: Int, changes: List[Int]): Int = {
      if(capacity == 0) 1
      else if(capacity < 0) 0
           else if(changes.isEmpty && capacity>=1 ) 0
                else count(capacity, changes.tail) + count(capacity - changes.head, changes)
    }
    count(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
