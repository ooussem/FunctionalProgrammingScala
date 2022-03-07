import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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