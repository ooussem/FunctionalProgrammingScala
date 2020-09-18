

val nums = 1 :: 2 :: 3 :: Nil

val numsSame = Nil.::(3).::(2).::(1)

nums match {
  case 1 :: 2 :: xs => print("toto")
  case x :: Nil => print("length = 1")
  case Nil => print("Nil")
}




def conditionInferior: (Int, Int) => Boolean = (a, b) => a < b
def insert(y: Int, tail: List[Int]): List[Int] = {
  tail match {
    case Nil => y :: Nil
    case x :: xs =>
      if(conditionInferior(y, x)) y :: x :: xs
      else x :: insert(y, xs)
  }
}


def insertionSort(listInt: List[Int]): List[Int] = listInt match {
  case List() => List()
  case x :: xs => insert(x, insertionSort(xs))
}

val numList = List(9, 4, 1, 2, 3, 6, 5)

insertionSort(Nil)
insertionSort(numList)


numList.flatMap{
  x => {
    List(x, "x2 = " + x * 2, "x3=" + x * 3)
  }
}

