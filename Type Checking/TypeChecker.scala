import Library._
import Parser._
import Typed._

case class NotImplementedException(s: String) extends RuntimeException(s)
case class DException() extends DesugarException()
case class IException() extends InterpException()
case class TException() extends TypeException()


object Desugar {
  def desugar(e: ExprExt): ExprC = {
    e match {
      case TrueExt() => TrueC()
      case FalseExt() => FalseC()
      case NumExt(x) => NumC(x)
      case NilExt(t) => NilC()
      case BinOpExt(s, l, r) => s match {
        case "+" => PlusC(desugar(l), desugar(r))
        case "*" => MultC(desugar(l), desugar(r))
        case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))
        case "and" => IfC(desugar(l), desugar(r), FalseC())
        case "or" => IfC(desugar(l), TrueC(), desugar(r))
        case "num=" => EqNumC(desugar(l), desugar(r))
        case  "num<" => LtC(desugar(l), desugar(r))
        case "num>" => LtC(desugar(r), desugar(l))
        case "cons" => ConsC(desugar(l), desugar(r))
        case "setbox" => SetboxC(desugar(l), desugar(r))
        case "seq" => SeqC(desugar(l), desugar(r))
      }
      case UnOpExt(s, e) => s match {
        case "-" => MultC(desugar(e), NumC(-1))
        case "not" => IfC(desugar(e), FalseC(), TrueC())
        case "head" => HeadC(desugar(e))
        case "tail" => TailC(desugar(e))
        case "is-nil" => IsNilC(desugar(e))
        // case "is-list" => IsListC(desugar(e))
        case "box" => BoxC(desugar(e))
        case "unbox" => UnboxC(desugar(e))
      }
      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))
      case ListExt(t, l) => helper(l)

      case IdExt(x) => IdC(x)
      case AppExt(f, args) => AppC(desugar(f), helperList(args))
      case LetExt(binds, body) => AppC(FdC(binds.map(x => x.name), desugar(body)), binds.map(x => x.value).map(desugar))
      case FdExt(params, body) => FdC(params.map(i => i.name), desugar(body))
      case RecLamExt(name, paramTy, retTy, param, body) => helperRec(name, param, body)
      case SetExt(id, e) => SetC(id, desugar(e))

      case ProjExt(n, e) => ProjC(n, desugar(e))
      case TupleExt(list) => TupleC(list.map(desugar))

      case LetRecExt(binds, body) =>
        val list = List.fill(binds.length)(UninitializedC())
        val result = helperLetRec(binds, body)
        AppC(FdC(binds.map(_.name), result),  list)
      case _ => UninitializedC()
    }
  }

  def helperLetRec(binds: List[LetRecBindExt], body: ExprExt): ExprC = {
    binds match {
      case (head :: Nil) => SeqC(SetC(head.name, desugar(head.value)), desugar(body))
      case (head :: tail) => SeqC(SetC(head.name, desugar(head.value)), helperLetRec(tail, body))
    }
  }

  def helperRec(name: String, param: String, body: ExprExt): ExprC = {
    AppC(FdC(List("f"),AppC(FdC(List("y"),AppC(IdC("y"),List(IdC("y")))),List(FdC(List("z"),AppC(IdC("f"),List(FdC(List("x"),AppC(AppC(IdC("z"),List(IdC("z"))),List(IdC("x")))))))))),
      List(FdC(name::Nil, FdC(param::Nil, desugar(body)))))
  }


  def helper(e: List[ExprExt]): ExprC = e match {
    case Nil => NilC()
    case (head :: tail) => ConsC(desugar(head), helper(tail))
    case _ => throw new DException()
  }

  def helperList(e: List[ExprExt]): List[ExprC] = e match {
    case Nil => Nil
    case h :: t => desugar(h) :: helperList(t)
  }
}

object Interp {
  type Store = List[Cell]
  type PointerEnvironment = List[Pointer]

  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: PointerEnvironment, st1: Store): (Value, Store) = {
    e match {
      case TrueC() => (BoolV(true), st1)
      case FalseC() => (BoolV(false), st1)
      case NilC() => (NilV(), st1)
      case NumC(x) => (NumV(x), st1)
      case UninitializedC() => (UninitializedV(), st1)
      case x@FdC(params, body) => (PointerClosV(x, nv), st1)
      case SetboxC(b, v) => interp(b, nv, st1) match {
        case (BoxV(value), store) => {
          val x = interp(v, nv, store)
          val resultSt = setboxHelper(value, x._1, x._2)
          (x._1, resultSt)
        }
        case _ => throw IException()
      }
      case BoxC(v) => {
        val x = interp(v, nv, st1)
        (BoxV(x._2.length), x._2 :+ Cell(x._2.length, x._1))
      }
      case UnboxC(b) => {
        interp(b, nv, st1) match {
          case (BoxV(v), st) => (unboxHelper(v, st),st)
          case _ => throw IException()
        }
      }
      case SeqC(b1, b2) => {
        // println("pavlos")
        // println(b1)
        // println(b2)
        val first = interp(b1, nv, st1)
        // println(first + " grjior")
        interp(b2, nv, first._2)
      }
      case SetC(v, b) => {
        // println("hello" + v)
        val isThere = nv.find(_.name == v).map(_.location).getOrElse(-999)
        if (isThere == -999) throw IException()
        val st = interp(b, nv, st1)
        val store = sethelper2(isThere, st._1, st._2)
        // println("finished")
        (st._1, store)
      }
      case PlusC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), store) => interp(r, nv, store) match {
          case (NumV(y), store1) => (NumV(x+y), store1)
          case _ => throw IException()
        }
        case _ => throw IException()
      }
      case MultC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), store) => interp(r, nv, store) match {
          case (NumV(y), store1) => (NumV(x*y), store1)
          case _ => throw IException()
        }
        case _ => throw IException()
      }
      case IfC(c, t, e) => interp(c, nv, st1) match {
        case (BoolV(false), store1) => interp(e, nv, store1)
        case (BoolV(true), store1) => interp(t, nv, store1)
        case _ => throw IException()
      }
      case EqNumC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), store) => interp(r, nv, store) match {
          case (NumV(y), store1) if y == x => (BoolV(true), store1)
          case (NumV(y), store1) => (BoolV(false), store1)
          case _ => throw IException()
        }
        case _ => throw IException()
      }

      case LtC(l, r) => interp(l, nv, st1) match {
        case (NumV(x), store) => interp(r, nv, store) match {
          case (NumV(y), store1) if y > x => (BoolV(true), store1)
          case (NumV(y), store1) => (BoolV(false), store1)
          case _ => throw IException()
        }
        case _ => throw IException()
      }

      case ConsC(l, r) => {
        val head = interp(l, nv, st1)
        val tail = interp(r, nv, head._2)
        (ConsV(head._1, tail._1), tail._2)
      }

      case HeadC(e) => interp(e, nv, st1) match {
        case (ConsV(head, tail), store) => (head, store)
        case _ => throw IException()
      }

      case TailC(e) => interp(e, nv, st1) match {
        case (ConsV(head, tail), store) => (tail, store)
        case _ => throw IException()
      }

      case IsNilC(e) => interp(e, nv, st1) match {
        case (NilV(), store) => (BoolV(true), store)
        case (ConsV(_, _), store) => (BoolV(false), store)
        case _ => throw IException()
      }

      case IdC(c) => (idhelper(c, nv, st1), st1)

      case AppC(f, args) => interp(f, nv, st1) match {
        case (PointerClosV(FdC(params, body), env), st2) if params.length == args.length => {
          val (environment, finalStore) = apphelper(params, args, nv, st2, Nil)
          interp(body, environment ::: env, finalStore)
        }
        case _ => throw new IException()
      }

      case TupleC(list) => {
        val emptyL = List[Value]()
        val fold = list.foldLeft(emptyL, st1) {
          case ((l, st), v) => {
            val (final1, final2) = interp(v, nv, st)
            val result1 = l ::: List(final1)
            (result1, final2)
          }
        }
        (TupleV(fold._1), fold._2)
      }

      case ProjC(i, c) => interp(c, nv, st1) match {
        case (TupleV(list), st) => {
          if (i < list.length) {
            (list(i), st)
          } else {
            throw IException()
          }
        }
        case _ => throw IException()
      }

      case _ => throw IException()
    }
  }

  def apphelper(params: List[String], args: List[ExprC], nv: PointerEnvironment,
                st1: Store, env: PointerEnvironment): (PointerEnvironment, Store) = (params, args) match {
    case (h1 :: t1, h2 :: t2) => {
      val (v, st2) = interp(h2, nv, st1)
      val location = st2.length
      apphelper(t1, t2, nv, edit(location, st2, v), Pointer(h1, location) :: env)
    }
    case (_ :: Nil, Nil) | (Nil, _ :: Nil) => throw new IException()
    case (Nil, Nil) => (env, st1)
  }


  def edit(location: Int, st: Store, value: Value): Store = st match {
    case Cell(l, _) :: t if l == location => Cell(l, value) :: t
    case c :: t => c :: edit(location, t, value)
    case Nil => Cell(location, value) :: Nil
  }


  def sethelper2(l: Int, value: Value, store: Store): Store = {
    val result = store.map{
      case x@Cell(length, y) if (length == l) => Cell(length, value)
      case x => x
    }
    result
  }

  def idhelper(c: String, env: PointerEnvironment, st: Store): Value = env match {
    case Nil => {
      // println("NIL")
      throw IException()
    }
    case Pointer(name, loc) :: t if (c == name) => {
      // println("not NIL")
      unboxHelper(loc, st)
    }
    case Pointer(name, loc) :: t => {
      // println("NOT NOT NIL")
      // println("c " + c + " name " + name)
      idhelper(c, t, st)
    }
  }

  def setboxHelper(n: Int, value: Value, store: Store): Store = {
    val result = store.map {
      case x@Cell(y, v) if (y == n) => Cell(y, value)
      case x => x
    }
    result
  }

  def unboxHelper(v: Int, st: Store): Value = {
    st match {
      case Nil => throw IException()
      case Cell(x, value)::tail if (x == v) => value
      case Cell(x, value)::tail if (x != v) => unboxHelper(v, tail)

    }
  }
}

object TypeChecker {
  type TEnvironment = List[TBind]

  def typeOf(e: ExprExt): Type = typeOf(e, Nil)

  def typeOf(e: ExprExt, nv: TEnvironment): Type = {
    e match {
      case TrueExt() => BoolT()
      case FalseExt() => BoolT()
      case NumExt(x) => NumT()
      case NilExt(t) => ListT(t)
      case BinOpExt(s, l, r) => {
        s match {
          case "*" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => NumT()
              case _ => throw TException()
            }
          }

          case "+" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => NumT()
              case _ => throw TException()
            }
          }

          case "-" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => NumT()
              case _ => throw TException()
            }
          }

          case "and" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (BoolT(), BoolT()) => BoolT()
              case _ => throw TException()
            }
          }

          case "or" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (BoolT(), BoolT()) => BoolT()
              case _ => throw TException()
            }
          }
          case "num=" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => BoolT()
              case _ => throw TException()
            }
          }

          case "num<" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => BoolT()
              case _ => throw TException()
            }
          }

          case "num>" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => BoolT()
              case _ => throw TException()
            }
          }

          case "cons" => {
            typeOf(r, nv) match {
              case ListT(x) => {
                if (typeOf(l, nv) == x) {
                  ListT(x)
                } else {
                  throw TException()
                }
              }
              case _ => throw TException()
            }
          }


          case "setbox" => {
            typeOf(l, nv) match {
              case RefT(x) if x == typeOf(r, nv) => x
              case _ => throw TException()
            }
          }

          case "seq" => {
            typeOf(l, nv)
            typeOf(r, nv)
          }
        }
      }

      case UnOpExt(s, e) => {
        s match {
          case "not" => typeOf(e, nv) match {
            case BoolT() => BoolT()
            case _ => throw TException()
          }
          case "-" => typeOf(e, nv) match {
            case NumT() => NumT()
            case _ => throw TException()
          }
          case "head" => typeOf(e, nv) match {
            case ListT(x) => x
            case _ => throw TException()
          }

          case "tail" => typeOf(e, nv) match {
            case ListT(x) => ListT(x)
            case _ => throw TException()
          }

          case "is-nil" => typeOf(e, nv) match {
            case ListT(x) => BoolT()
            case _ => throw TException()
          }

          case "box" => RefT(typeOf(e, nv))

          case "unbox" => typeOf(e, nv) match {
            case RefT(x) => x
            case _ => throw TException()
          }
        }
      }

      case IfExt(c, t, e) =>
        val cType = typeOf(c, nv)
        val tType = typeOf(t, nv)
        val eType = typeOf(e, nv)

        (cType, tType, eType) match {
          case (BoolT(), `tType`, `eType`) => tType
          case _ => throw TException()
        }

      case ListExt(l, es) => {
        if (listhelper(l, nv, es) == 1) {
          ListT(l)
        } else {
          throw TException()
        }
      }

      case TupleExt(l) => l match {
        case Nil => TupleT(Nil)
        case head :: tail => TupleT(typeOf(head, nv) :: tail.map(typeOf(_, nv)))
      }

      case ProjExt(n, e) => typeOf(e, nv) match {
        case TupleT(list) if (list.size > n && n >= 0) => list(n)
        case _ => throw TException()
      }

      case FdExt(params, body) => {
        val paramTypes = params.map(i => i.ty)
        val newEnv = params.map(i => TBind(i.name, i.ty)) ::: nv
        val returnType = typeOf(body, newEnv)
        FunT(paramTypes, returnType)
      }

      case IdExt(x) => idhelper(x, nv)

      case SetExt(id, e) => {
        (idhelper(id, nv), typeOf(e, nv)) match {
          case (t, ext) => if (t == ext) ext else throw TException()
          case _ => throw TException()
        }
      }

      case AppExt(f, args) => typeOf(f, nv) match {
        case FunT(t1, t2) => {
          val types = args.map(x => typeOf(x, nv))
          if (t1.equals(types)) {
            t2
          } else {
            throw TException()
          }
        }
        case _ => throw TException()
      }

      case LetExt(binds, body) => {
        val newEnv = binds.collect {
          case LetBindExt(name, value) => TBind(name, typeOf(value, nv))
        }
        typeOf(body, nv ::: newEnv)
      }

      case RecLamExt(name, paramTy, retTy, param, body) =>
        val env = TBind(param, paramTy) :: TBind(name, FunT(List(paramTy), retTy)) :: nv
        typeOf(body, env) match {
          case `retTy` => typeOf(IdExt(name), env)
          case _ => throw new TException()
        }

      case LetRecExt(binds, body) => {
        val newEnv = nv ++ binds.map(x => TBind(x.name, x.ty))
        val t1 = binds.map(x => x.ty)
        val t2 = binds.map(x => typeOf(x.value, newEnv))
        if (t1 == t2) {
          typeOf(body, newEnv)
        } else {
          throw TException()
        }
      }



      case _ => throw TException()
    }
  }

  def idhelper(string: String, nv: TEnvironment): Type = {
    nv match {
      case TBind(name, typ) :: t => if (name == string) typ else idhelper(string, t)
      case _ => throw TException()
    }
  }

  def listhelper(typ: Type, nv: TEnvironment, es: List[ExprExt]): Int = {
    es match {
      case h :: t => if (typeOf(h, nv) == typ) listhelper(typ, nv, t) else -1
      case Nil => 1
      case _ => throw TException()
    }
  }
}

object SafeInterp {

  def interp(e: ExprExt): Value = {
    TypeChecker.typeOf(e)
    Interp.interp(Desugar.desugar(e))
  }

}