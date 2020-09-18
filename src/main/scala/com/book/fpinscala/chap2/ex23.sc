def add: (Int, Int) => Int = {
  (x: Int, y: Int) => x + y
}

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  a => (b => f(a, b))
}
curry(add)(2)(4)

//ex 24
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}
uncurry(curry(add))(2, 4)

//ex25
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}

