// Preliminary Part about the 3n+1 conjecture
//============================================

object CW6a {
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long): Long = {
   require(n >= 1 && n<= 1000000)

  def increment(n: Long, length: Long): Long ={
    if (n <= 1) length
    else if (n % 2 == 0) increment(n / 2, length + 1)
    else increment(3 * n + 1, length + 1)}
    
  increment(n, 0)
}

//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
  require(bnd >= 1 && bnd <= 1000000)         
  
   val lst2 = for (i <- (1L to bnd)) yield collatz(i) 
   val maxNum = lst2.max
  
  (maxNum,lst2.indexOf(maxNum)+1)
}

//(3) Implement a function that calculates the last_odd
//    number in a collatz series.  For this implement an
//    is_pow_of_two function which tests whether a number 
//    is a power of two. The function is_hard calculates 
//    whether 3n + 1 is a power of two. Again you can
//    assume the input ranges between 1 and 1 Million,
//    and also assume that the input of last_odd will not 
//    be a power of 2.
def is_pow_of_two(n: Long) : Boolean = {if (n%2==0) true else false}

def odd(n:Long):Boolean={
  if (is_pow_of_two(n)==false) true
  else false
}
def is_hard(n: Long) : Boolean = {
  val y = (3*n)+1
  if (y % 2==0 )true else false}

def last_odd(n: Long) : Long = {
    require(n>=1 && n<=1000000)
      def increment(n: Long, lst:List[Long]) : List[Long] =
        if(n == 1) lst :+ n
        else if(n % 2 == 0) increment(n/2, lst :+ n)
        else increment(n * 3  + 1, lst :+ n)

    val collatzList:List[Long]=increment(n,Nil)
    val oddList:List[Long] = collatzList.filter(odd)
    oddList(oddList.size-2)
}
}