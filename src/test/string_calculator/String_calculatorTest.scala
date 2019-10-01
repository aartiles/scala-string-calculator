package string_calculator

import org.scalatest._
import org.scalatest.Matchers._

final class String_calculatorTest extends WordSpec with GivenWhenThen {
  "String_calculator" should {
    "add" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers is an empty string")

      val numbers = ""
      val result = string_calculator.add(numbers)

      Then("it should return 0")

      result shouldBe "0"
    }

    "add a single number" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers is a single number")

      val numbers = "3"
      val result = string_calculator.add(numbers)

      Then("it should return the same number")

      result shouldBe "3.0"
    }

    "add a two numbers" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has two numbers")

      val numbers = "3,2"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of both numbers")

      result shouldBe "5.0"
    }

    "add unlimited numbers" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has many numbers")

      val numbers = "3,5.2,4,2.1"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of all the numbers")

      result shouldBe "14.3"
    }

    "add accept \\n as numbers separator" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers separated by \\n")

      val numbers = "3,5.2,4\n2.1"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of all the number")

      result shouldBe "14.3"
    }

    "add identifies separator at invalid position" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers have two consecutive separators")

      val numbers = "175.2,\n35"
      val result = string_calculator.add(numbers)

      Then("it should return an error message")

      result shouldBe "Number expected but '\n' found at position 6."
    }

    "add don’t allow the input to end in a separator" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers end in a separator")

      val numbers = "1,3,"
      val result = string_calculator.add(numbers)

      Then("it should return an error message")

      result shouldBe "Number expected but EOF found"
    }

    "add parser delimiter" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has a delimiter definition")

      val numbers = "//,\n1,3"
      val result = string_calculator.add(numbers)

      Then("it should sum the numbers")

      result shouldBe "4.0"
    }

    "add parser take into account a non default delimiter" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has a non default delimiter definition")

      val numbers = "//;\n1;2"
      val result = string_calculator.add(numbers)

      Then("it should sum the numbers")

      result shouldBe "3.0"
    }

    "add parser take into account a non default multi-character delimiter" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has a non default multi-character delimiter definition")

      val numbers = "//sep\n2sep3"
      val result = string_calculator.add(numbers)

      Then("it should sum the numbers")

      result shouldBe "5.0"
    }

  }
}
