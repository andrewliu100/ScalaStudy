package exercise.part5pm

object PatternMatch extends App {

  /*
  takes an Expr => human readable form

    Sum(Number(2), Number(3)) => 2 + 3
    Sum(Sum(Number(2), Number(3)), Number(4)) => 2 + 3 + 4
    Prod(Sum(Number(2), Number(3)), Number(4)) => (2 + 3) * 4
    Prod(Prod(Number(2), Number(3)), Number(4)) => 2 * 3 * 4
   */

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def show(e: Expr): String = e match {
    case Number(n) => s"$n"
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) => {
      def showProd(expr: Expr) = expr match {
        case Number(_) => show(expr)
        case Prod(_, _) => show(expr)
        case _ => "(" + show(expr) + ")"
      }
      showProd(e1) + " * " + showProd(e2)
    }
  }

  println(show(Prod(Sum(Number(2), Number(3)), Sum(Number(4), Number(5)))))
  println(show(Sum(Sum(Number(2), Number(3)), Number(4))))
}
