sealed trait Token
case class Number(value: Double) extends Token

sealed trait Operator extends Token
case object Add extends Operator
case object Subtract extends Operator
case object Multiply extends Operator
case object Divide extends Operator
