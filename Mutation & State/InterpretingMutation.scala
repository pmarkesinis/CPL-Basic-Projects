import Library._
import Parser._
import Untyped._

case class NotImplementedException(s: String) extends RuntimeException(s)
case class DException() extends DesugarException()
case class IException(s: String) extends InterpException(s: String)
case class InterpFormatException(s: String) extends InterpException(s: String)
case class DesFormatException(s: String) extends DesugarException(s: String)

object Desugar {
  def desugar(e: ExprExt): ExprC = {
    e match {
      case TrueExt() => TrueC()
      case FalseExt() => FalseC()
      case NilExt() => NilC()
      case NumExt(x) => NumC(x)
      case BinOpExt(s, l ,r) => s match {
        case "+" => PlusC(desugar(l), desugar(r))
        case "*" => MultC(desugar(l), desugar(r))
        case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))
        case "and" => IfC(desugar(l), desugar(r), FalseC())
        case "or" => IfC(desugar(l), TrueC(), desugar(r))
        case "num=" => EqNumC(desugar(l), desugar(r))
        case "num<" => LtC(desugar(l), desugar(r))
        case "num>" => LtC(MultC(NumC(-1), desugar(l)), MultC(NumC(-1), desugar(r)))
        case "cons" => ConsC(desugar(l), desugar(r))
        case "setbox" => SetboxC(desugar(l), desugar(r))
        case "seq" => SeqC(desugar(l), desugar(r))
        case _ => throw new DException()
      }
      case UnOpExt(s, e) => s match {
        case "-" => MultC(NumC(-1), desugar(e))
        case "not" => IfC(desugar(e), FalseC(), TrueC())
        case "head" => HeadC(desugar(e))
        case "tail" => TailC(desugar(e))
        case "is-nil" => IsNilC(desugar(e))
        case "is-list" => IsListC(desugar(e))
        case "box" => BoxC(desugar(e))
        case "unbox" => UnboxC(desugar(e))
      }
      case SetExt(id, e) => SetC(id, desugar(e))
      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
      case LetExt(binds, body) =>
        AppC(
          FdC(binds.map(x => x.name), desugar(body)), binds.map(x => x.value).map(desugar)
        )
      case RecLamExt(name, param, body) => AppC(FdC(List("f"),
        AppC(FdC(List("y"),
          AppC(IdC("y"),List(IdC("y")))),
          List(FdC(List("z"),
            AppC(IdC("f"),
              List(FdC(List("x"),
                AppC(AppC(IdC("z"),List(IdC("z"))),
                  List(IdC("x"))
                )
              )
              )
            )
          )
          )
        )
      ), List(FdC(List(name), FdC(List(param), desugar(body)))))
      case IdExt(c) => IdC(c)
      case AppExt(f, args) => AppC(desugar(f), args.map(desugar))
      case FdExt(params, body) => FdC(params, desugar(body))
      case CondExt(cs) => nestedIf(cs)
      case CondEExt(cs, e) => nestedIf(cs, e)
      case ListExt(list) => listHelper(list)
      case LetRecExt(binds, body) => {
        val names = binds.map(x => x.name)
        val uninitialized = binds.map(x => UninitializedC())
        AppC(
          FdC(names, sequenceHelper(binds, body)), uninitialized
        )
      }
      case _ => UninitializedC()
    }
  }

  def sequenceHelper(binds: List[LetBindExt], body: ExprExt): ExprC = {
    binds match {
      case Nil => desugar(body)
      case h :: t => SeqC(SetC(h.name, desugar(h.value)), sequenceHelper(t, body))
    }
  }

  def listHelper(list: List[ExprExt]): ExprC = {
    list match {
      case Nil => NilC()
      case h :: t => ConsC(desugar(h), listHelper(t))
    }
  }

  def nestedIf(cs: List[(ExprExt, ExprExt)]): ExprC = {
    cs match {
      case Nil => UndefinedC()
      case h :: t => IfC(desugar(h._1), desugar(h._2), nestedIf(t))
    }
  }
  def nestedIf(cs: List[(ExprExt, ExprExt)], e: ExprExt): ExprC = {
    cs match {
      case Nil => desugar(e)
      case h :: t => IfC(desugar(h._1), desugar(h._2), nestedIf(t, e))
    }
  }

  def helperList(e: List[ExprExt]): ExprC = e match {
    case Nil => NilC()
    case (a :: rest) => ConsC(desugar(a), helperList(rest))
    case _ => throw DesFormatException("Invalid input")
  }

  def helperLetRec(binds: List[LetBindExt], body: ExprExt): ExprC = binds match {
    case (bind :: Nil)  => SeqC(SetC(bind.name, desugar(bind.value)), desugar(body))
    case (bind :: rest)  => SeqC(SetC(bind.name, desugar(bind.value)), helperLetRec(rest, body))
  }

  def helper(name: String, param: String, body: ExprExt): ExprC = {
    AppC(FdC(List("f"),AppC(FdC(List("y"),AppC(IdC("y"),List(IdC("y")))),List(FdC(List("z"),AppC(IdC("f"),List(FdC(List("x"),AppC(AppC(IdC("z"),List(IdC("z"))),List(IdC("x")))))))))), List(FdC(name::Nil, FdC(param::Nil, desugar(body)))))
  }
}

object Interp {
  type Store = List[Cell]
  type PointerEnvironment = List[Pointer]

  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: PointerEnvironment, st1:  Store): (Value, Store) = {
    // println(e)
    e match {
      case TrueC() => (BoolV(true), st1)
      case FalseC() => (BoolV(false), st1)
      case NumC(x) => (NumV(x), st1)
      case NilC() => (NilV(), st1)
      case UninitializedC() => (UninitializedV(), st1)
      case PlusC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), st2) => interp(r, nv, st2) match {
          case (NumV(y), st3) => (NumV(x+y), st3)
          case _ => throw new IException("Plus1")
        }
        case _ => throw new IException("Plus2")
      }
      case MultC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), st2) => interp(r, nv, st2) match {
          case (NumV(y), st3) => (NumV(x*y), st3)
          case _ => throw new IException("Mult1")
        }
        case _ => throw new IException("Mult2")
      }
      case IfC(c, t, e) => interp(c, nv, st1) match {
        case (BoolV(true), st2) => interp(t, nv, st2)
        case (BoolV(false), st2) => interp(e, nv, st2)
        case _ => throw new IException("if")
      }
      case EqNumC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), st2) => interp(r, nv, st2) match {
          case (NumV(y), st3) => (BoolV(x==y), st3)
          case _ => throw new IException("eqnum1")
        }
        case _ => throw new IException("eqnum2")
      }
      case LtC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), st2) => interp(r, nv, st2) match {
          case (NumV(y), st3) => (BoolV(x<y), st3)
          case _ => throw new IException("ltc1")
        }
        case _ => throw new IException("ltc2")
      }
      case ConsC(l, r) => {
        val left = interp(l, nv, st1)
        val right = interp(r, nv, left._2)
        (ConsV(left._1, right._1), right._2)
      }
      case HeadC(e) => interp(e, nv, st1) match {
        case (ConsV(hd, tl), st2) => (hd, st2)
        case _ => throw new IException("head")
      }
      case TailC(e) => interp(e, nv, st1) match {
        case (ConsV(hd, tl), st2) => (tl, st2)
        case _ => throw new IException("tail")
      }
      case IsNilC(e) => interp(e, nv, st1) match {
        case (NilV(), st2) => (BoolV(true), st2)
        case (_, st2) => (BoolV(false), st2)
      }

      case IsListC(e) => interp(e, nv, st1) match {
        case (NilV(), st2) => (BoolV(true), st2)
        case (ConsV(_, _), st2) => (BoolV(true), st2)
        case (_, st2) => (BoolV(false), st2)
      }

      case IdC(x) => {
        val location = lookUp(x, nv)
        (findValue(location, st1), st1)
      }

      case x@FdC(params, body) => (PointerClosV(x, nv), st1)
      case AppC(f, args) => interp(f, nv, st1) match {
        case (PointerClosV(f1, nv1), st2) => {
          if (args.length != f1.params.length) throw new IException("AppC1 - diff lengths")
          val argValsAndStore = getArgValsAndStore(args, nv, st2, List())
          val newEnv = getCells(nv1, argValsAndStore._2, f1.params.zip(argValsAndStore._1))
          val res = interp(f1.body, newEnv._1, newEnv._2)
          res
        }
        case _ => throw new IException("AppC")
      }
      case SeqC(b1, b2) => {
        val st2 = interp(b1, nv, st1)._2
        interp(b2, nv, st2)
      }
      case SetC(v, b) => {
        val location = lookUp(v, nv)
        val body = interp(b, nv, st1)
        val store = updateStore(location, body._2, body._1)
        (body._1, store)
      }

      case BoxC(v) => {
        val value = interp(v, nv, st1)
        val cell = value._2.length
        (BoxV(cell), Cell(cell, value._1) +: value._2)
      }
      case UnboxC(b) => interp(b, nv, st1) match {
        case (BoxV(x), st2) => {
          val value = findValue(x, st2)
          (value, st2)
        }
        case _ => throw new IException("unbox")
      }
      case SetboxC(b, v) => interp(b, nv, st1) match {
        case (BoxV(x), st2) => {
          val body = interp(v, nv, st2)
          val st3 = updateStore(x, body._2, body._1)
          (body._1, st3)
        }
        case _ => throw new IException("SETBOX")
      }
      case _ => (UninitializedV(), st1)
    }
  }

  def getArgValsAndStore(args: List[ExprC], nv: PointerEnvironment, st1: Store, res: List[Value]): (List[Value], Store) = {
    args match {
      case Nil => (Nil, st1)
      case (param :: Nil) =>
        val value = interp(param, nv, st1)
        val test = res :+ value._1
        (test, value._2)
      case (param :: rest) =>
        val value = interp(param, nv, st1)
        getArgValsAndStore (rest, nv, value._2, res :+ value._1)
    }}

  def getCells(nv: PointerEnvironment, st1: Store, pv: List[(String, Value)]): (PointerEnvironment, Store) = pv match {
    case Nil => (nv, st1)
    case ((param, value) :: Nil) =>
      val cell = st1.length
      val newStore = st1 :+ Cell(cell, value)
      val newEnv = Pointer(param, cell) +: nv
      (newEnv, newStore)
    case ((param, value) :: rest) =>
      val cell = st1.length
      val newStore = st1 :+ Cell(cell, value)
      val newEnv = Pointer(param, cell) +: nv
      getCells(newEnv, newStore, rest)
    case _ => throw InterpFormatException("Cell not found")
  }

  def updateEnvironment(binds: List[(String, Value)], st: Store, nv: PointerEnvironment): PointerEnvironment = {
    binds match {
      case Nil => nv
      case h :: t => {
        val pointer = Pointer(h._1, findLocation(h._2, st))
        updateEnvironment(t, st, nv :+ pointer)
      }
    }
  }

  def findValue(loc: Int, st: Store): Value = {
    st match {
      case Nil => throw new IException("findValue")
      case h :: t if h.location == loc => h.value
      case h :: t => findValue(loc, t)
    }
  }

  def updateStore2(args: List[Value], st1: List[Cell]): List[Cell] = {
    args match {
      case Nil => st1
      case h :: t => {
        val cell = Cell(st1.length, h)
        updateStore2(t, st1 :+ cell)
      }
    }
  }

  def updateStore(loc: Int, st1: List[Cell], body: Value): List[Cell] = {
    st1 match {
      case Nil => Nil
      case h :: t if h.location == loc => Cell(loc, body) :: t
      case h :: t => h :: updateStore(loc, t, body)
    }

  }

  def lookUp(v: String, nv: List[Pointer]): Int = {
    // println("ENVIRONMENT" + nv)
    nv match {
      case Nil => throw new IException("lookup")
      case h :: t if h.name == v => h.location
      case h :: t => lookUp(v, t)
    }
  }

  def findLocation(v: Value, st: Store): Int = {
    st match {
      case Nil => throw new IException("findLocation")
      case h :: t if v == h.value => h.location
      case h :: t => findLocation(v, t)
    }
  }

  def lookup(name: String, env: PointerEnvironment, st1: Store): Value = env match {
    case Nil => throw InterpFormatException("Invalid input 14")
    case Pointer(n, location) :: tail if (n == name) => lookupCell(location, st1)
    case Pointer(n, location) :: tail => lookup(name, tail, st1)
  }

  def lookupCell(i: Int, st1: Store): Value = st1 match {
    case Nil => throw InterpFormatException("Invalid input 15")
    case Cell(n, v) :: tail if (n == i) => v
    case Cell(n, v) :: tail => lookupCell(i, tail)
  }

  def updateCell(loc: Int, st: Store, v: Value): Store = {

    val store = st.map {
      case cell @ Cell(n, value) if n == loc => Cell(n, v)
      case cell => cell
    }
    store
  }
}