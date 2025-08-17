object PolishCalculator {

  private def parseToken(str: String): Option[Token] = str match {
    case "+" => Some(Add)
    case "-" => Some(Subtract)
    case "*" => Some(Multiply)
    case "/" => Some(Divide)
    case numStr => numStr.toDoubleOption.map(double => Number.apply(double))
  }

  private def applyOperator(op: Operator, a: Double, b: Double): Double = op match {
    case Add => a + b
    case Subtract => a - b
    case Multiply => a * b
    case Divide => a / b
  }

  private def processToken(token: Token, stack: List[Double]): Either[String, List[Double]] = token match {
    case Number(value) =>
      Right(value :: stack)

    case op: Operator =>
      stack match {
        case b :: a :: rest => Right(applyOperator(op, a, b) :: rest)
        case _ => Left(s"Not enough operands for $op")
      }
  }

  private def parseInput(input: List[String]): Either[String, List[Token]] = {
    val tokenResults = input.map(parseToken)

    if (tokenResults.forall(_.isDefined)) {
      Right(tokenResults.map(_.get))
    } else {
      Left("Invalid token in input")
    }
  }

  private def evaluate(tokens: List[Token]): Either[String, Double] = {
    val initialStack: Either[String, List[Double]] = Right(List.empty)

    tokens.foldLeft(initialStack) { (stackEither, token) =>
      stackEither.flatMap(stack => processToken(token, stack))
    }.flatMap {
      case result :: Nil => Right(result)
      case Nil => Left("No result - empty expression")
      case _ => Left("Invalid expression - multiple values remain")
    }
  }

  def calculate(input: List[String]): Either[String, Double] = {
    for {
      tokens <- parseInput(input)
      result <- evaluate(tokens)
    } yield result
  }
}
