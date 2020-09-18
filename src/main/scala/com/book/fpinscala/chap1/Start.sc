val numbers = List((1, 2), (3, 4))
val numbersFill = List.fill(2)(numbers)
//val numbersFillUnzip = numbersFill.unzip

//List.fill(5)(numbers).flatMap(l => l.map(x => (x, x*2, x*3)))


val x = new StringBuilder("Hello")
val y = x.append(", World")
val r1 = y.toString
val r2 = x.toString
