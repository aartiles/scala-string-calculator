package string_calculator


class Parser {
  val SEPARATORS = Array('\n', ',')

  def parse(input: String): Array[Double] = {
    var numbers = Array.empty[Double]
    var position = 0
    do {
      val (currentPosition, token) = this.nextToken(position, input)
      if (token.isEmpty) {
        position = currentPosition - 1
        val c = input.charAt(position)
        throw new RuntimeException(s"Number expected but '$c' found at position $position.")
      }

      numbers = numbers :+ token.toDouble
      position = currentPosition
    }
    while (position < input.length)
    numbers
  }

  private def nextToken(initialPosition: Int, input: String): (Int, String) = {
    var currentPosition = initialPosition
    var number = ""
    do {
      val c = input.charAt(currentPosition)
      if (this.isSeparator(c) && currentPosition == input.length - 1) {
        throw new RuntimeException("Number expected but EOF found")
      }
      if (this.isSeparator(c)) return (currentPosition + 1, number)
      number += c
      currentPosition += 1
    }
    while (currentPosition < input.length)
    (currentPosition, number)
  }

  private def isSeparator(c: Char): Boolean = SEPARATORS contains c

}
