import Library._
import Parser._
import Untyped._

case class NotImplementedException(s: String) extends RuntimeException(s)
case class DException() extends DesugarException
case class IException() extends InterpException

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

      case IdExt(x) => IdC(x)
      case AppExt(f, args) => AppC(desugar(f), helperList(args))
      case LetExt(binds, body) => AppC(FdC(binds.map(x => x.name), desugar(body)), binds.map(x => x.value).map(desugar))
      case FdExt(params, body) => FdC(params, desugar(body))
      case RecLamExt(name, param, body) => helperRec(name, param, body)
      case _ => throw new DException()
    }
  }

  def helperRec(name: String, param: String, body: ExprExt): ExprC = {
    AppC(FdC(List("f"),AppC(FdC(List("y"),AppC(IdC("y"),List(IdC("y")))),List(FdC(List("z"),AppC(IdC("f"),List(FdC(List("x"),AppC(AppC(IdC("z"),List(IdC("z"))),List(IdC("x")))))))))),
      List(FdC(name::Nil, FdC(param::Nil, desugar(body)))))
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

  def helperList(e: List[ExprExt]): List[ExprC] = e match {
    case Nil => Nil
    case h :: t => desugar(h) :: helperList(t)
  }
}

object Interp {
  def interp(e: ExprC, nv: Environment): Value = {
    println(nv)
    e match {
      case NilC() => NilV()
      case NumC(x) => NumV(x)
      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)

      case PlusC(l, r) => (interp(l, nv), interp(r, nv)) match {
        case (NumV(x), NumV(y)) => NumV(x+y)
        case _ => throw new IException()
      }
      case MultC(l, r) => (interp(l, nv), interp(r, nv)) match {
        case (NumV(x), NumV(y)) => NumV(x*y)
        case _ => throw new IException()
      }
      case IfC(a,b,c) => interp(a, nv) match {
        case BoolV(true) => interp(b, nv)
        case BoolV(false) => interp(c, nv)
        case _ => throw new IException()
      }
      case EqNumC(l, r) => (interp(l, nv), interp(r, nv)) match {
        case (NumV(x), NumV(y)) if (x == y) => BoolV(true)
        case (NumV(x), NumV(y)) if (x != y) => BoolV(false)
        case _ => throw new IException()
      }

      case LtC(l, r) => (interp(l, nv), interp(r, nv)) match {
        case (NumV(x), NumV(y)) => BoolV(x < y)
        case _ => throw new IException()
      }

      case ConsC(h, t) => ConsV(interp(h, nv), interp(t, nv))

      case HeadC(e) => interp(e, nv) match {
        case ConsV(x, _) => x
        case _ => throw new IException()
      }

      case TailC(e) => interp(e, nv) match {
        case ConsV(_, y) => y
        case _ => throw new IException()
      }

      case IsNilC(e) => interp(e, nv) match {
        case NilV() => BoolV(true)
        case ConsV(_, _) => BoolV(false)
        case _ => throw new IException()
      }

      case IsListC(e) => interp(e, nv) match {
        case NilV() => BoolV(true)
        case ConsV(_, _) => BoolV(true)
        case _ => BoolV(false)
      }

      case IdC(c) => checkEnv(c, nv)
      case x@FdC(params, body) => ClosV(x, nv)

      case AppC(f, args) => interp(f, nv) match {
        case ClosV(FdC(params, body), newEnv) =>
          if (args.length != params.length) {
            throw new IException()
          }
          val values = args.map(interp(_, nv))
          val binds = params.zip(values).map {case (n,v) => Bind(n,v)}
          interp(body, binds ::: newEnv)
        case _ => throw IException()
      }

      case _ => throw new IException()
    }
  }

  def checkEnv(c: String, env: Environment): Value = {
    env match {
      case Nil => throw IException()
      case Bind(name, value) :: t if (name == c) => value
      case Bind(name, value) :: t => checkEnv(c, t)
    }
  }

  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil)
}