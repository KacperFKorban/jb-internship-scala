//> using scala 3.5.0
//> using dep "org.scalameta::munit:1.0.2"

/**
  * The complexity of this funciton is O(2^n)
  * For a given integer n, the function computes 2^n. we can just implement is as:
  * def f(x: Int): Int = Math.pow(2, x)
  */
def f(x: Int): Int = x match
  case 0 => 1
  case x => f(x - 1) + f(x - 1)

def fBetter(x: Int): Int = Math.pow(2, x).toInt

class warmuptest extends munit.FunSuite:
  test("f == fBetter") {
    for i <- 0 to 10 do
      assertEquals(f(i), fBetter(i))
  }
