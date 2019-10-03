package string_calculator

case class Delimiters(delimiters: List[String]) {
  private val delimitersRegexp = delimiters.mkString("|")

  def buildWithNewDelimiters(numbers: String): Delimiters = {
    if (numbers.startsWith("//")) {
      val newDelimiter = numbers.split("\n").head.replace("//", "")
      Delimiters(delimiters ++ List(newDelimiter))
    } else {
      this
    }
  }

  def cleanControlChars(numbers: String) = {
    if (numbers.startsWith("//")) {
      numbers.split("\n").takeRight(1).mkString(",")
    } else {
      numbers
    }
  }

  def splitByDelimiter(aString: String): List[String] = {
    aString.split(delimitersRegexp).toList
  }

  def endsWithDelimiter(aString: String): Boolean = {
    delimiters.count(aString.endsWith) > 0
  }
}
