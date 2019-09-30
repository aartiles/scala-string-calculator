package string_calculator

final class String_calculator {
  val SEPARATORS = Array('\n', ',')

  def add(input: String): String = {
    if (input.isEmpty) return "0"

    val numbers = this.parse(input)
    println("numbers", numbers.foreach(println))
    println("is separator", isSeparator('\n'))
    val sum = numbers.sum
    println("sum", sum)
    this.format(sum)
  }

  private def parse(input: String): Array[Double] = {
    var numbers = Array.empty[Double]
    var position = 0
    do {
      val (currentPosition, token) = this.nextToken(position, input)
      if (token.isEmpty) {
        val c = input.charAt(currentPosition)
        throw new RuntimeException(s"Number expected but $c found at position $currentPosition.")
      }
      println("next token", currentPosition, token.toDouble)

      numbers = numbers :+ token.toDouble
      println("numbers._0", numbers(0))

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
      if (this.isSeparator(c)) return (currentPosition + 1, number)
      number += c
      currentPosition += 1
    }
    while (currentPosition < input.length)
    return (currentPosition, number)
  }

  private def isSeparator(c: Char): Boolean = SEPARATORS contains c

  private def format(n: Double): String = this.roundAt(2)(n).toString

  private def roundAt(p: Int)(n: Double): Double = {
    val s = math pow (10, p); (math round n * s) / s
  }
}
