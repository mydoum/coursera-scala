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
      if (c == 0 || c == r) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def b(subChars: List[Char], op: Int): Boolean = {
        if (op < 0) false
        else if (subChars.isEmpty) op == 0
        else if (subChars.head == '(')  b(subChars.tail, op + 1)
        else if (subChars.head == ')')  b(subChars.tail, op - 1)
        else b(subChars.tail, op)
      }
    b(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
