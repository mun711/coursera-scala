package forcomp

import scala.collection.mutable.ListBuffer
import forcomp.Anagrams._

object Main{
  def main(args: Array[String]){
    val s = List("yes","man")
    val result = Anagrams.sentenceAnagrams(s)
    println(result.distinct.foreach { println })
  }  
}