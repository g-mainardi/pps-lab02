package task2

import scala.annotation.tailrec

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

  printSection("4")
  def getTuple3String(x: Int, y: Int, z: Int) = "[" + x + ", " + y + ", " + z + "]"
  def testMultipleValues(testValues: (Boolean, Int, Int, Int) => Unit) =
    testValues(true, ZERO_VALUE, VALUE_1, VALUE_1)
    testValues(false, ZERO_VALUE, VALUE_1, VALUE_2)
    testValues(true, VALUE_1, VALUE_1, VALUE_1)
    testValues(false, VALUE_1, ZERO_VALUE, VALUE_2)
  def testRelation(relation: (Int, Int, Int) => Boolean, style: String): Unit =
    def testValues(expected: Boolean, x: Int, y: Int, z: Int): Unit =
      println("Test relation on " + getTuple3String(x, y, z) + " in "+ style + " style: " + getResultString(expected, relation(x, y, z)))

    testMultipleValues(testValues)

  val relationFunLit: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  testRelation(relationFunLit, "Function Literal")

  def relationMethod(x: Int, y: Int, z: Int): Boolean = x <= y && y == z
  testRelation(relationMethod, "Method")

  def testCurriedRelation(relation: Int => Int => Int => Boolean, style: String): Unit =
    def testValues(expected: Boolean, x: Int, y: Int, z: Int): Unit =
      println("Test curried relation on " + getTuple3String(x, y, z) + " in " + style + " style: " + getResultString(expected, relation(x)(y)(z)))

    testMultipleValues(testValues)

  val relationCurriedFunLit: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  testCurriedRelation(relationCurriedFunLit, "Curried Function Literal")

  def relationCurriedMethod(x: Int) (y: Int) (z: Int): Boolean = x <= y && y == z
  testCurriedRelation(relationCurriedMethod, "Curried Method")

  printSection("5")
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  val function1: Int => Int = _ - 1
  val function2: Int => Int = _ * 2
  val function3: Int => Int = compose(function1, function2)
  println("Test compose f(g(x)) on [" + VALUE_1 + "] => " + function3(VALUE_1))

  def genericCompose[X, GX, FGX](f: GX => FGX, g: X => GX): X => FGX = (x: X) => f(g(x))
  val genericFunction3: Int => Int = genericCompose(function1, function2)
  println("Test generic compose f(g(x)) on [" + VALUE_1 + "] => " + genericFunction3(VALUE_1))

  val boolToString: Boolean => String =
    case true => "true"
    case false => "false"
  val isZeroString: Int => String = genericCompose(boolToString, isZero)
  println(VALUE_1 + " == 0? " + isZeroString(VALUE_1))
  println(ZERO_VALUE + " == 0? " + isZeroString(ZERO_VALUE))

  printSection("6")
  val applyZero: (Int => Int) => Int = f => f(ZERO_VALUE)

  def testOnIsAppliedZeroString(str: String, composeFunction: (Boolean => String, Int => Boolean, (Int => Int) => Int) => (Int => Int) => String): Unit =
    val isAppliedZeroString: (Int => Int) => String =
      composeFunction(boolToString: Boolean => String, isZero: Int => Boolean, applyZero: (Int => Int) => Int)

    println("Test " + str + " on isAppliedZeroString: " + getResultString(true, isAppliedZeroString(function1) == "false"))

  def testOnEsclamateDouble(str: String, composeFunction: (String => String, Int => String, Int => Int) => Int => String): Unit =
   val esclamateDouble: Int => String = composeFunction(_ + "!", _.toString, _ * 2)

    println("Test " + str + " on esclamateDouble: " + getResultString(true, esclamateDouble(VALUE_1) == "6!"))

  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = (a: A) => f(g(h(a)))
  def recycledComposeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = (a: A) => genericCompose(f, genericCompose(g, h))(a)

  testOnIsAppliedZeroString("composeThree", composeThree)
  testOnIsAppliedZeroString("recycledComposeThree", composeThree)

  testOnEsclamateDouble("composeThree", composeThree)
  testOnEsclamateDouble("recycledComposeThree", composeThree)

  printSection("7")
  def testPower(str: String, powerFunction: (Double, Int) => Double): Unit =
    val DOUBLE_1 = 2.0
    val DOUBLE_2 = 5.0
    val EXPONENT_1 = 2
    val EXPONENT_2 = 3

    def testWithValues(base: Double, exponent: Int): Unit =
      println("Test " + str + " with [" + base + "^(" + exponent + ")] => " + powerFunction(base, exponent))

    testWithValues(DOUBLE_1, EXPONENT_1)
    testWithValues(DOUBLE_1, EXPONENT_2)
    testWithValues(DOUBLE_2, EXPONENT_1)
    testWithValues(DOUBLE_2, EXPONENT_2)

  def power(base: Double, exponent: Int): Double = exponent match
    case n if n > 0 => base * power(base, exponent - 1)
    case n if n == 0 => 1.0
    case _ => 0.0

  testPower("Recursive Power", power)

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(exponent: Int, acc: Double): Double = exponent match
      case 0 => 1.0
      case 1 => acc
      case _ => _power(exponent - 1, base * acc)

    _power(exponent, 1.0)

  testPower("Recursive Tail Power", powerTail)

  printSection("8")
  def testEquals(expected: Int, number: Int): Unit =
    testTrue(number, n => n == expected)
  val NUMBER1 = 12345
  val NUMBER2 = 80711
  val NUMBER3 = 100
  val NUMBER1_REVERSE = 54321
  val NUMBER2_REVERSE = 11708
  val NUMBER3_REVERSE = 1

  def dropLastDigit(n: Int): Int = n / 10
  println("Test dropLastDigit ")
  testEquals(123, dropLastDigit(1234))
  testEquals(1, dropLastDigit(10))

  def getLastDigit(n: Int): Int = n % 10
  println("Test getLastDigit ")
  testEquals(4, getLastDigit(1234))
  testEquals(0, getLastDigit(10))

  def appendDigit(n: Int, digit: Int): Int = n * 10 + digit
  println("Test appendDigit ")
  testEquals(1234, appendDigit(123, 4))
  testEquals(2, appendDigit(0, 2))
  testEquals(0, appendDigit(0, 0))
  testEquals(10, appendDigit(1, 0))

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, acc: Int): Int = n match
      case 0 => acc
      case _ => _reverse(dropLastDigit(n), appendDigit(acc, getLastDigit(n)))

    _reverse(n, 0)

  println("Test reverseNumber ")
  testEquals(NUMBER1_REVERSE, reverseNumber(NUMBER1))
  testEquals(NUMBER2_REVERSE, reverseNumber(NUMBER2))
  testEquals(NUMBER3_REVERSE, reverseNumber(NUMBER3))

  printSection("9 (Test in dedicated package) ")
  enum Expr:
    case Literal(constant: Int)
    case Add(subExpr1: Expr, subExpr2: Expr)
    case Multiply(subExpr1: Expr, subExpr2: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Literal(c) => c
      case Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

    def show(expr: Expr): String = expr match
      case Literal(c) => "" + c
      case Add(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
      case Multiply(e1, e2) => "(" + show(e1) + " * " + show(e2) + ")"