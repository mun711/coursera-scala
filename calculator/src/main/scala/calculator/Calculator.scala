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
    namedExpressions map {
      case (v, e) => (v, Signal(eval(e(), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    
    def evalF(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
      case Literal(v) => v
      case Ref(name) => evalF(getReferenceExpr(name, references), references)
      case Plus(a,b) => evalF(a , references) + evalF(b , references)
      case Minus(a,b) => evalF(a , references) - evalF(b , references)
      case Times(a,b) => evalF(a , references) * evalF(b , references)
      case Divide(a,b) => evalF(a , references) / evalF(b , references)
    }

    def isCyclicRef(origEx: Expr): Boolean = {
      
      def checkRefs(origExpr: Expr, expr: Expr):Boolean = expr match { 
        case Literal(v) => false
        case Ref(name) => {
          val refExpr = getReferenceExpr(name,references)
          if (refExpr == origExpr) true
          else checkRefs(origExpr, refExpr)
        }
        case Plus(a, b) => checkRefs(origExpr, a) || checkRefs(origExpr, b)        
        case Minus(a, b) => checkRefs(origExpr, a) || checkRefs(origExpr, b)
        case Times(a, b) => checkRefs(origExpr, a) || checkRefs(origExpr, b)
        case Divide(a, b) => checkRefs(origExpr, a) || checkRefs(origExpr, b)
      }
      
      checkRefs(origEx, origEx)
    }
            
    if (isCyclicRef(expr)) Double.NaN
    else evalF(expr, references)
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
