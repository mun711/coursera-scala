package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  /*
  var un: Set = union(singletonSet(3),singletonSet(7))
  printSet(map(un, x => x*2))
  
  val s1: Set = singletonSet(1)
  val s2: Set = singletonSet(2)

  printSet(intersect(s1,s2))
  */
}
