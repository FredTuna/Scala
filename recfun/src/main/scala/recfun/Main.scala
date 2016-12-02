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
	  if(c > r) 0
	  else {
	    if(c == 0) 1
		else pascal(c-1, r-1) + pascal(c, r-1)
	  }
	}
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
	
      def balanceWithDepth(chars: List[Char], depth: Int): Boolean = {
	    if(chars.isEmpty) depth == 0 
	    else if(chars.head == '(') balanceWithDepth(chars.tail, depth + 1)
	    else if(chars.head == ')') {
	      if(depth == 0) false
		  else balanceWithDepth(chars.tail, depth - 1)
	    }
	    else balanceWithDepth(chars.tail, depth)
	  }
	
	  balanceWithDepth(chars, 0)
	}
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
	  
	  if(coins.isEmpty || money < 0) 0
	  else if(money == 0) 1
	  else {
	    if(money >= coins.head) countChange(money - coins.head, coins) + countChange(money, coins.tail)
		else countChange(money, coins.tail)
	  }
	}
  }
