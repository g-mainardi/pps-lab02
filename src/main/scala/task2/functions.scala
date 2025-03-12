package task2

object functions extends App:
  def printSection(section: String): Unit = println("\n----------------"+section+"----------------\n")

  printSection("3a")
  val positiveFunLit: Int => String =
    case x if x >= 0 => "positive"
    case _ => "negative"

  def positiveMethod(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  val POS_VALUE = 5
  val NEG_VALUE = -4
  println("(i) Function Literal style(pos): " + positiveFunLit(POS_VALUE))
  println("(i) Function Literal style(neg): " + positiveFunLit(NEG_VALUE))

  println("(ii) Method style(pos): " + positiveMethod(POS_VALUE))
  println("(ii) Method style(neg): " + positiveMethod(NEG_VALUE))

  printSection("3b")
  val empty: String => Boolean = _ == ""
  val STRING1 = "foo"
  val STRING2 = "deeee"
  val EMPTY_STRING = ""

  def getResultString(expected: Boolean, result: Boolean): String = result match
    case result if expected==result => "ok"
    case _ => "no"

  def testValue[X](value: X, predicate: X => Boolean, expected: Boolean): Unit =
    println("Test '" + value + "' : " + getResultString(expected, predicate(value)))

  def testTrue[X](value: X, predicate: X => Boolean): Unit = testValue(value, predicate, true)
  def testFalse[X](value: X, predicate: X => Boolean): Unit = testValue(value, predicate, false)

  def testStringNeg(style: String, neg: (String => Boolean) => String => Boolean): Unit =
    println("Neg on String with " + style + " style")
    val newNegEmpty = neg(empty)
    testTrue(STRING1, newNegEmpty)
    testTrue(STRING2, newNegEmpty)
    testFalse(EMPTY_STRING, newNegEmpty)

  val negFunLit: (String => Boolean) => String => Boolean =
    (predicate: String => Boolean) => s => !predicate(s)
  testStringNeg("Function Literal", negFunLit)

  def negMethod(predicate: String => Boolean): String => Boolean =
    def negPredicate(str: String): Boolean = str match
      case _ => !predicate(str)
    negPredicate
  testStringNeg("Function Method", negMethod)

  printSection("3c")
  def negGeneric[X](predicate: X => Boolean) =
    def negPredicate(value: X): Boolean = value match
      case _ => !predicate(value)

    negPredicate

  testStringNeg("Generics Method", negGeneric[String])
  println("Neg on Integer with Generics Method style")
  val isZero: Int => Boolean = _ == 0
  val negIsZero = negGeneric(isZero)
  val VALUE_1 = 3
  val VALUE_2 = -7
  val ZERO_VALUE = 0
  testTrue(VALUE_1, negIsZero)
  testTrue(VALUE_2, negIsZero)
  testFalse(ZERO_VALUE, negIsZero)
