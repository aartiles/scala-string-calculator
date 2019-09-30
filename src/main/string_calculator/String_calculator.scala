package string_calculator

final class String_calculator {

  def add(input: String): String = {
    if (input.isEmpty) return "0"
    val parser = new Parser()
    try {
      val numbers = parser.parse(input)
      val sum = numbers.sum
      this.format(sum)
    } catch {
      case ex: RuntimeException => return ex.getMessage
    }
  }

  private def format(n: Double): String = this.roundAt(2)(n).toString

  private def roundAt(p: Int)(n: Double): Double = {
    val s = math pow (10, p); (math round n * s) / s
  }
}
