
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

def ifNonStrict[A](cond: Boolean, funcTrue: => A, funcFalse: => A): A = {
  if(cond) funcTrue else funcFalse
}




@tailrec
def retry[T](delay: Int, iteration: Int)(funcToTry: => Try[T]): Try[T] = {
  funcToTry match {
    case Success(value) => Success(value)
    case Failure(exception) => {
      if (iteration > 1 ) {
        println("Retry : iteration = " + iteration)
        Thread.sleep(delay)
        retry(delay, iteration - 1)(funcToTry)
      } else {
        throw exception
      }
    }
  }
}

//retry(5, 3)(Try(5 / 0))


def testCatch(numb: Int, otherNumb: Int)(f: (Int, Int) => Int) = {
  Try(f(numb, otherNumb)) match {
    case Failure(exception) => {
      exception.printStackTrace()
    }
    case Success(value) => value
  }
}

testCatch(3, 0)((x, y) => x / y)
testCatch(15, 5)((x, y) => x / y)







def maybeTwice(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}

maybeTwice(true, {println("Hi"); 10})


val empt: com.book.fpinscala.chap5.Stream[Int] = com.book.fpinscala.chap5.Empty
val stream1 = com.book.fpinscala.chap5.Cons(() => {println("Hi"); 10}, () => empt)
val stream2 = com.book.fpinscala.chap5.Stream.cons( {println("Hi"); 10}, empt)

stream1.headOption
stream1.headOption

stream2.headOption
stream2.headOption