package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.foreach(println)
    namedExpressions.map { case (str, sig) => (str, Signal(eval(sig(), str, namedExpressions))) }
  }

  def eval(expr: Expr, name: String, references: Map[String, Signal[Expr]]): Double = {
    def evalWithStack(expr: Expr, dependencyList: List[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(str) => {
          if (!references.contains(str)) Double.NaN
          else if (dependencyList.contains(str)) {
            references.filter { case (strRef, _) => dependencyList.contains(strRef) }.foreach { case (strRef, sig) => eval(sig(), strRef, references)}
            Double.NaN
          }
          else evalWithStack(references(str)(), str::dependencyList)
        }
        case Plus(a, b) => evalWithStack(a, dependencyList) + evalWithStack(b, dependencyList)
        case Minus(a, b) => evalWithStack(a, dependencyList) - evalWithStack(b, dependencyList)
        case Times(a, b) => evalWithStack(a, dependencyList) * evalWithStack(b, dependencyList)
        case Divide(a, b) => evalWithStack(a, dependencyList) / evalWithStack(b, dependencyList)
        case _ => Double.NaN
      }
    }

    evalWithStack(expr, List(name))
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
