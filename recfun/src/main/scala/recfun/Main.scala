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
    	if (r <= 1) return 1
    	if (c == 0 || c == r) return 1
    	return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      	def balance(parans: List[Char] , chars: List[Char]): Boolean = {
      		if (chars.isEmpty && !parans.isEmpty) return false
      		if (chars.isEmpty && parans.isEmpty) return true
      		val ch = chars.head
      		if (ch != '(' && ch != ')') return balance(parans, chars.tail)
      		if (ch == '(') return balance(parans.::(ch) , chars.tail)
      			else if (ch == ')' && parans.isEmpty) return false
      			else if (ch == ')' && parans.head == '(') return balance(parans.tail , chars.tail)
      			else if (ch == ')') return false
      		return balance(parans,chars)
    	}
    
        def balance2(chars: List[Char] , count: Int): Boolean = {
        	if (chars.isEmpty) count == 0
       		else {
       			val ch = chars.head
       			val n = 
       				if (ch == '(') count+1
       				else if (ch == ')') count-1
       				else count
       			if (n >= 0) balance2(chars.tail, n)
       			else false
	       }
    	}
    
    	return balance2(chars , 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    	def c(money: Int, coins: List[Int]): Int = {
      		if (money == 0) 1
      		else if (money > 0 && !coins.isEmpty){
        	c(money - coins.head , coins) + c(money , coins.tail)
      	}
      	else 0
    	}    
	c(money, coins)
    }

  }
