package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  
   val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  
  printSet(s1)
  printSet(s2)
  printSet(s3)
  
  val s4 = union(s1, s2)
  printSet(s4)
  val s5 = union(s4, s3)
  printSet(s5)
  
  val s6 = intersect(s4, s5)
  printSet(s6)
  println("******")  
  printSet(filter(s5, s1))
  printSet(filter(s5, s6))
  printSet(filter(s5, s3))
  println("*****")  
  printSet(map(s5, x => x * x))
}
