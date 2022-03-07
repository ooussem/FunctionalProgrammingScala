
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

def ifNonStrict[A](cond: Boolean, funcTrue: => A, funcFalse: => A): A = {
  if(cond) funcTrue else funcFalse
}


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
