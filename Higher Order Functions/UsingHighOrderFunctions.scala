import java.util.NoSuchElementException

object Solution {

  /**
   * Define the function lengths(xs), which returns a list of the lengths of the strings in the list xs.
   * Example: if the input is List("I", "took", "a", "quick", "glimpsing", "at", "bright", "stars"),
   * the output should be List(1, 4, 1, 5, 9, 2, 6, 5).
   *
   * You MUST use a high-order function and you CANNOT use loops.
   *
   * After you solve this exercise using high-order function, compare your solution to the one you made
   * in the previous assignment using mutation and loops.
   */
  def lengths(xs: List[String]): List[Int] = {
    xs.map(_.length)
  }

  /**
   * Define the function longWords(words), which returns the words that have more than 5 characters.
   *
   * You MUST use a high-order function and you CANNOT use loops.
   */
  def longWords(words: List[String]): List[String] = {
    words.filter(_.length > 5)
  }

  /**
   * Define the function maximum(numbers), which uses foldLeft to return the highest number
   * in the given list of non-negative numbers, and -1 if the list is empty.
   *
   * You MUST use a high-order function and you CANNOT use loops.
   */
  def maximum(numbers: List[Int]): Int = {
    numbers.foldLeft(-1)((max, curr) => {
      if (max > curr) max
      else curr
    })
  }
}
