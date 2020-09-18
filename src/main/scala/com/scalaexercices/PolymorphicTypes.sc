import scala.collection.immutable

trait Animal {
  def name: String
}
case class Reptile(name: String) extends Animal
trait Mammal extends Animal
case class Zebra(name: String) extends Mammal
case class Giraffe(name: String) extends Mammal

def selection(a1: Animal, a2: Animal): (Animal, Animal) = {
  println(a1)
  println(a2)
  (a1, a2)
}
selection(Reptile("reptile"), Giraffe("giraffe"))

def selection2[A <: Animal](a1: A, a2: A): (A, A) = {
  println(a1)
  println(a2)
  (a1, a2)
}

selection2(Reptile("reptile"), Giraffe("giraffe"))
selection2(Reptile("reptile1"), Reptile("reptile2"))


def selection3[A >: Animal](a1: A, a2: A): (A, A) = {
  println(a1)
  println(a2)
  (a1, a2)
}

selection3(Reptile("reptile"), Giraffe("giraffe"))
selection3(Reptile("reptile1"), Reptile("reptile2"))

// Problem of Miskov
val zebras: Array[Mammal] = Array(Zebra("z1"))
val mammals: Array[Mammal] = zebras
mammals(0) = Giraffe("g1")
val zebra: Mammal = zebras(0)



trait Function1[-T, +U] {
  def apply(x: T): U
}

trait Vet[-A] {
  def treat(a: A): Unit
}

case class VetM(id: Int) extends Vet[Mammal] {
  override def treat(a: Mammal): Unit = ???
}

case class VetZ(id: Int) extends Vet[Mammal] {
  override def treat(a: Mammal): Unit = ???
}


trait StreamAnimal[T] {
  def prepend[U >: T](elem: U): Stream[U] =
    Stream.cons(elem, this)
}





