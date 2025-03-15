package task4

import org.junit.*
import org.junit.Assert.*
import Arithmetic.*
import Expr.*

class ArithmeticTest:
  val VALUE1: Int = 8
  val VALUE2: Int = 13

  val literal1 = Literal(VALUE1)
  val literal2 = Literal(VALUE2)

  @Test def testEvaluate(): Unit =
    assertEquals(VALUE1, evaluate(literal1))
    assertEquals(VALUE1 + VALUE2, evaluate(Add(literal1, literal2)))
    assertEquals(VALUE1 * VALUE2, evaluate(Multiply(literal1, literal2)))

  @Test def testShow(): Unit =
    val str1 = "" + VALUE1
    val strSum = "(" + VALUE1 + " + " + VALUE2 + ")"
    val strProd = "(" + VALUE1 + " * " + VALUE2 + ")"
    assertEquals(str1, show(literal1))
    assertEquals(strSum, show(Add(literal1, literal2)))
    assertEquals(strProd, show(Multiply(literal1, literal2)))

