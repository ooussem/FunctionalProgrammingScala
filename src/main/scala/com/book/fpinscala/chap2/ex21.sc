import scala.annotation.tailrec

def fib(n: Int): Int = {
  @tailrec
  def loop(n: Int, acc: Int, cur: Int): Int = {
    if(n <= 0) acc
    else loop(n - 1, cur, cur + acc)
  }
  loop(n, 0, 1)
}


fib(5)

