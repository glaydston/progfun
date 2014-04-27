object session {
  1 + 2
  def abs(x: Double) = if (x < 0) -x else x

  def srqt(x: Double) = {
    def srqtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else srqtIter(improve(guess))
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001
    def improve(guess: Double) =
      (guess + x / guess) / 2
    srqtIter(1.0)
  }
  srqt(2)
  srqt(4)
  srqt(1e-6)
  srqt(1e60)

  //   Lecture 1.7 - Tail Recursion (12:32)
  def gcd(a: Int, b: Int): Int =
    if(b == 0) a else gcd(b, a%b)

  gcd(21, 14)

  //def factorial(n: Int): Int =
  //  if(n == 0) 1 else n * factorial(n -1)

  //factorial(4)

  // Tail Recursion in Scala

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if(n == 0) acc
      else loop(acc * n, n -1)
      loop(1, n)
   }

  factorial(4)


}