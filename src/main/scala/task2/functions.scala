package task2

object functions extends App:
  def getSectionString(section: String): String = "\n----------------"+section+"----------------\n"

  println(getSectionString("3a"))
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

  println(getSectionString("3b"))
  val empty: String => Boolean = _ == ""
  val STRING1 = "foo"
  val STRING2 = "deeee"
  val EMPTY_STRING = ""

  def getResultString(expected: Boolean, result: Boolean): String = result match
    case result if expected==result => "ok"
    case _ => "no"

  def testString(str: String, stringPredicate: String => Boolean, expected: Boolean):String =
    "Test '" + str + "' : " + getResultString(expected, stringPredicate(str))

  def testTrue(str: String, stringPredicate: String => Boolean): String = testString(str, stringPredicate, true)
  def testFalse(str: String, stringPredicate: String => Boolean): String = testString(str, stringPredicate, false)

  println("Neg with Function Literal style")
  val negFunLit: (String => Boolean) => (String => Boolean) =
    (predicate: String => Boolean) => (s => !predicate(s))

  val notEmptyByFunLit: String => Boolean = negFunLit(empty)

  println(testTrue(STRING1, notEmptyByFunLit))
  println(testTrue(STRING2, notEmptyByFunLit))
  println(testFalse(EMPTY_STRING, notEmptyByFunLit))

  println("Neg with Function Method style")
  def negMethod(predicate: String => Boolean): String => Boolean =
    def negPredicate(str: String): Boolean = str match
      case _ => !predicate(str)
    negPredicate

  val notEmptyByMethod: String => Boolean = negFunLit(empty)

  println(testTrue(STRING1, notEmptyByMethod))
  println(testTrue(STRING2, notEmptyByMethod))
  println(testFalse(EMPTY_STRING, notEmptyByMethod))
