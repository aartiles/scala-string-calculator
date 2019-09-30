package string_calculator

final class String_calculator {
  def add(input: String): String = {
    if (input.isEmpty) return "0"

    val numbers = this.parse(input)
    val sum = numbers.sum
    this.format(sum)
  }

  private def parse(input: String): Array[Double] = {
    input.reduce
    input.split(",|\n")
      .map((n: String) => if (n.isEmpty) "0" else n)
      .map(_.toDouble)
  }

  private def format(n: Double): String = this.roundAt(2)(n).toString

  private def roundAt(p: Int)(n: Double): Double = {
    val s = math pow (10, p); (math round n * s) / s
  }
}
