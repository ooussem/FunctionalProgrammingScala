import scala.annotation.tailrec

def sumInts(a: Int, b: Int): Int = {
  if(a > b) 0 else a + sumInts(a+1, b)
}

def factorial(n: Int): Int = {
  if(n < 0 || n == 0) 1
  else n * factorial(n - 1)
}

factorial(3)

@tailrec
def factorialTailRec(x: Int, result: Int): Int = {
  if(x == 1) result
  else factorialTailRec(x - 1, x * result)
}
factorialTailRec(3, 1)

def sumFactorials(a: Int, b: Int): Int =
  if (a > b) 0
  else factorial(a) + sumFactorials(a + 1, b)


