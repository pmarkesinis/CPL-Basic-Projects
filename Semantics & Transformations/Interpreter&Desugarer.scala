import Library._
import Parser._
import Untyped._

case class NotImplementedException(s: String) extends RuntimeException(s)
case class DException() extends DesugarException()
case class IException() extends InterpException()

object Desugar {
  def desugar(e: ExprExt): ExprC = {
    e match {
      case TrueExt() => TrueC()
      case FalseExt() => FalseC()
      case NumExt(x) => NumC(x)
      case NilExt() => NilC()

      case BinOpExt("+", l, r) => PlusC(desugar(l), desugar(r))
      case BinOpExt("*", l, r) => MultC(desugar(l), desugar(r))
      case BinOpExt("-", l, r) => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))
      case BinOpExt("and", l, r) => IfC(desugar(l), desugar(r), FalseC())
      case BinOpExt("or", l, r) => IfC(desugar(l), TrueC(), desugar(r))
      case BinOpExt("num=", l, r) => EqNumC(desugar(l), desugar(r))
      case BinOpExt("num>", l, r) => LtC(desugar(r), desugar(l))
      case BinOpExt("num<", l, r) => LtC(desugar(l), desugar(r))
      case BinOpExt("cons", l, r) => ConsC(desugar(l), desugar(r))

      case UnOpExt("-", e) => MultC(desugar(e), NumC(-1))
      case UnOpExt("not", e) => IfC(desugar(e), FalseC(), TrueC())
      case UnOpExt("head", e) => HeadC(desugar(e))
      case UnOpExt("tail", e) => TailC(desugar(e))
      case UnOpExt("is-nil", e) => IsNilC(desugar(e))
      case UnOpExt("is-list", e) => IsListC(desugar(e))

      case IfExt(a, b, c) => IfC(desugar(a), desugar(b), desugar(c))
      case ListExt(e) => helper(e)
      case CondExt(e) => helper1(e, UndefinedC())
      case CondEExt(e, el) => helper1(e, desugar(el))
      case _ => throw new DException()
    }
  }
  def helper(e: List[ExprExt]): ExprC = e match {
    case Nil => NilC()
    case (h :: t) => ConsC(desugar(h), helper(t))
    case _ => throw new DException()
  }

  def helper1(e: List[(ExprExt, ExprExt)], el: ExprC): ExprC = e match {
    case ((x,y) :: Nil) => IfC(desugar(x), desugar(y), el)
    case ((x,y) :: t) => IfC(desugar(x), desugar(y), helper1(t, el))
    case _ => throw new DException()
  }
}



object Interp {
  def interp(e: ExprC): Value = {
    e match {
      case NilC() => NilV()
      case NumC(x) => NumV(x)
      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)

      case PlusC(l, r) => (interp(l), interp(r)) match {
        case (NumV(x), NumV(y)) => NumV(x+y)
        case _ => throw new IException()
      }
      case MultC(l, r) => (interp(l), interp(r)) match {
        case (NumV(x), NumV(y)) => NumV(x*y)
        case _ => throw new IException()
      }
      case IfC(a,b,c) => interp(a) match {
        case BoolV(true) => interp(b)
        case BoolV(false) => interp(c)
        case _ => throw new IException()
      }
      case EqNumC(l, r) => (interp(l), interp(r)) match {
        case (NumV(x), NumV(y)) if (x == y) => BoolV(true)
        case (NumV(x), NumV(y)) if (x != y) => BoolV(false)
        case _ => throw new IException()
      }

      case LtC(l, r) => (interp(l), interp(r)) match {
        case (NumV(x), NumV(y)) => BoolV(x < y)
        case _ => throw new IException()
      }

      case ConsC(h, t) => ConsV(interp(h), interp(t))

      case HeadC(e) => interp(e) match {
        case ConsV(x, _) => x
        case _ => throw new IException()
      }

      case TailC(e) => interp(e) match {
        case ConsV(_, y) => y
        case _ => throw new IException()
      }

      case IsNilC(e) => interp(e) match {
        case NilV() => BoolV(true)
        case ConsV(_, _) => BoolV(false)
        case _ => throw new IException()
      }

      case IsListC(e) => interp(e) match {
        case NilV() => BoolV(true)
        case ConsV(_, _) => BoolV(true)
        case _ => BoolV(false)
      }

      case UndefinedC() => throw new IException()
      case _ => throw new IException()

    }
  }
}