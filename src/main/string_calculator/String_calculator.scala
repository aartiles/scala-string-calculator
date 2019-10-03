package string_calculator

import string_calculator.exceptions.{DuplicateDelimitersException, InvalidEOFException, InvalidLengthException}

class String_calculator(delimiters: Delimiters = Delimiters(List(",", "\\n"))) {

  def add(numbers: String): String = {
    try {
      val newDelimiters = delimiters.buildWithNewDelimiters(numbers)
      val calculator    = new Calculate(newDelimiters)
      val aResult       = calculator.run(numbers)
      format(aResult)
    } catch {
      case e: InvalidLengthException       => "0"
      case e: DuplicateDelimitersException => e.msg
      case e: InvalidEOFException          => e.msg
    }
  }

  private def format(number: Double): String = {
    BigDecimal(number).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString()
  }
}
