package string_calculator

object Parser {
  def create(input: String): Parser = {
    val delimiterRegExp = """^//(.+)\n""".r.unanchored
    input match {
      case delimiterRegExp(delimiter) => {
        println("delimiter", delimiter)
        println("new input", input.replaceAll(delimiterRegExp.toString, ""))
        new Parser(input.replaceAll(delimiterRegExp.toString, ""), delimiter)
      }
      case _ => {
        println("no match", input)
        new Parser(input)
      }
    }
  }
}

final class Parser(val input: String, val delimiter: String = null) {
  val DELIMITERS = Array('\n', ',')

  def parse: Array[Double] = {
    var numbers = Array.empty[Double]
    var position = 0
    do {
      val (currentPosition, token) = this.nextToken(position)
      if (token.isEmpty) {
        position = currentPosition - 1
        val c = this.input.charAt(position)
        throw new RuntimeException(s"Number expected but '$c' found at position $position.")
      }

      numbers = numbers :+ token.toDouble
      position = currentPosition
    }
    while (position < input.length)
    numbers
  }

  private def nextToken(initialPosition: Int): (Int, String) = {
    var currentPosition = initialPosition
    var number = ""
    do {
      val c = this.input.charAt(currentPosition)
      if (this.isDelimiter(c) && currentPosition == this.input.length - 1) {
        throw new RuntimeException("Number expected but EOF found")
      }
      if (this.isDelimiter(c)) return (currentPosition + 1, number)
      number += c
      currentPosition += 1
    }
    while (currentPosition < this.input.length)
    (currentPosition, number)
  }

  private def isDelimiter(c: Char): Boolean = {
    if (c.toString == this.delimiter) true else DELIMITERS contains c
  }

}
