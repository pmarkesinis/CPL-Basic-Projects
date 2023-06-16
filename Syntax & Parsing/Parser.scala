case class NotImplementedException(s: String) extends RuntimeException(s)
case class PException() extends ParseException()

object Parser {

  def parse(str: String): ExprExt = parse(Reader.read(str))

  def parse(sexpr: SExpr): ExprExt = sexpr match {

    case SNum(num) => NumExt(num)

    case SList(SSym(sym) :: args) if sym == "list" =>
      val parsedArgs = args.map(parse)
      ListExt(parsedArgs)

    case SList(SSym(sym) :: rest) if sym == "cond" => rest match {
      case values :+ SList(List(SSym("else"), elseCond)) if values.size > 0 =>
        CondEExt(values.map {
          case SList(List(val1, val2)) => (parse(val1), parse(val2))
          case _ => throw new PException()
        }, parse(elseCond))
      case values if values.size > 0 => CondExt(values.map {
        case SList(List(val1, val2)) => (parse(val1), parse(val2))
        case _ => throw new PException()
      })
      case _ => throw new PException()
    }

    case SSym(sym) => sym match {
      case "true" => TrueExt()
      case "false" => FalseExt()
      case "nil" => NilExt()
      case _ => throw new PException()
    }

    case SList(SSym(sym) :: expr :: Nil) => sym match {
      case "-" => UnOpExt("-", parse(expr))
      case "not" => UnOpExt("not", parse(expr))
      case "head" => UnOpExt("head", parse(expr))
      case "tail" => UnOpExt("tail", parse(expr))
      case "is-nil" => UnOpExt("is-nil", parse(expr))
      case "is-list" => UnOpExt("is-list", parse(expr))
      case _ => throw new PException()
    }



    case SList(SSym(sym) :: left :: right :: Nil) => sym match {
      case "and" => BinOpExt("and", parse(left), parse(right))
      case "or" => BinOpExt("or", parse(left), parse(right))
      case "cons" => BinOpExt("cons", parse(left), parse(right))
      case "+" => BinOpExt("+", parse(left), parse(right))
      case "-" => BinOpExt("-", parse(left), parse(right))
      case "*" => BinOpExt("*", parse(left), parse(right))
      case "num=" => BinOpExt("num=", parse(left), parse(right))
      case "num<" => BinOpExt("num<", parse(left), parse(right))
      case "num>" => BinOpExt("num>", parse(left), parse(right))
      case _ => throw new PException()
    }

    case SList(SSym(sym) :: left :: middle :: right :: Nil) => sym match {
      case "if" => IfExt(parse(left), parse(middle), parse(right))
      case _ => throw new PException()
    }

    case _ => throw new PException()
  }
}
