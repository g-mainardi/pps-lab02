package task2

object functions extends App:
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