import java.util.NoSuchElementException
import scala.collection.mutable.ListBuffer

object Solution {

  /**
   * Define the function lengths(xs), which returns a list of the lengths of the strings in the list xs.
   * Example: if the input is List("I", "took", "a", "quick", "glimpsing", "at", "bright", "stars"),
   * the output should be List(1, 4, 1, 5, 9, 2, 6, 5).
   *
   * Hint: ListBuffer is a mutable list in Scala that allows appending elements.
   *
   * You MUST use loops and CANNOT use high-order functions.
   */
  def lengths(xs: List[String]): List[Int] = {
    var result = new ListBuffer[Int]()
    for (i <- 0 until xs.size) {
      result += xs(i).length()
    }
    result.toList
  }
}
