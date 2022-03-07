def add: (Int, Int) => Int = {
  (x: Int, y: Int) => x + y
}

def curry[A, B, C](f: (A, B) => C): A => B => C = {
  a => b => f(a, b)
}
curry(add)(2)(4)

def curry3[A, B, E, C](f: (A, B, E) => C): A => B => E => C = {
  a => b => e => f(a, b, e)
}
def add3: (Int, Int, Int) => Int = (x: Int, y: Int, g: Int) => x + y + g
def add3def(a: Int, b: Int, c: Int): Int = a + b +c
curry3(add3)(1)(2)(3)

//ex 24
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}
uncurry(curry(add))(2, 4)

//ex25
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}



