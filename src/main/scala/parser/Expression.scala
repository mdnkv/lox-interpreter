package dev.mednikov.loxscala
package parser

import scanner.Token

sealed trait Expression

case class Binary(left: Expression, operator: Token, right: Expression) extends Expression

case class Grouping(expression: Expression) extends Expression

case class Literal(value: Any) extends Expression

case class Unary(operator: Token, right: Expression) extends Expression

case class Variable(name: Token) extends Expression

case class Assign(name: Token, value: Expression) extends Expression
