import scala.annotation.tailrec

def isSorted[A](as: Array[A], greaterThan: (A, A) => Boolean): Boolean = {
  @tailrec
  def sorted(n: Int): Boolean = {
    if(n >= as.length - 1) true
    else if(greaterThan(as(n), as(n+1))) false
    else sorted(n+1)
  }
  sorted(0)
}


val arrayNumber: Array[Int] = Array(1, 2, 2, 7, 1)
isSorted(arrayNumber, {(x: Int, y: Int) => x > y})



