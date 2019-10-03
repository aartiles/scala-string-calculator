package string_calculator

import string_calculator.exceptions.{DuplicateDelimitersException, InvalidEOFException, InvalidLengthException}

class Calculate(delimiters: Delimiters) {
  def run(numbers: String): Double = {
    val cleanNumbers = delimiters.cleanControlChars(numbers)
    isValid(cleanNumbers)
    val splittedNumbers = delimiters.splitByDelimiter(cleanNumbers)
    splittedNumbers.map(_.toDouble).sum
  }

  private def isValid(numbers: String): Unit = {
    isValidLength(numbers)
    isValidDuplicateDelimiters(numbers)
    isValidEOF(numbers)
  }

  private def isValidLength(numbers: String): Unit = {
    if (numbers.length == 0) {
      throw InvalidLengthException("Invalid Length")
    }
  }

  private def isValidEOF(numbers: String): Unit = {
    if (delimiters.endsWithDelimiter(numbers)) {
      throw InvalidEOFException("Number expected but EOF found")
    }
  }

  private def isValidDuplicateDelimiters(numbers: String): Unit = {
    val splittedNumbers = delimiters.splitByDelimiter(numbers)
    if (splittedNumbers.nonEmpty && splittedNumbers.count(_.length == 0) > 0) {
      val duplicateDelimitersPosition = splittedNumbers.map(_.length).takeWhile(_ > 0).sum + 1
      val duplicateDelimiterChar      = numbers.charAt(duplicateDelimitersPosition)
      throw DuplicateDelimitersException(
        s"Number expected but '$duplicateDelimiterChar' found at position $duplicateDelimitersPosition.")
    }
  }
}
