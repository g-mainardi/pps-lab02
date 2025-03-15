package task4

object Arithmetic:
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
