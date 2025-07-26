package dev.mednikov.loxscala
package parser

import scanner.{Token, TokenType}

import dev.mednikov.loxscala.errors.ParsingError
import dev.mednikov.loxscala.scanner.TokenType.*

import scala.collection.mutable.ListBuffer


class Parser(tokens: List[Token]) {

  private var current: Int = 0

  def parse(): List[Statement] = {
    try {
      val statements = ListBuffer.empty[Statement]
      while(!isAtEnd){
        statements += declaration()
      }
      statements.toList
    } catch {
      case e: ParsingError => null
    }
  }

  private def declaration(): Statement = {
    try {
      if (matchNext(VAR)) then return varDeclaration()
      statement()
    } catch {
      case e: ParsingError => {
        synchronize()
        null
      }
    }
  }

  private def varDeclaration(): Statement = {
    val name: Token = consume(IDENTIFIER, "Expect variable name.")
    val initializer: Expression = if (matchNext(EQUAL)) then expression() else null
    consume(SEMICOLON, "Expect ; after variable declaration")
    VarStatement(name, initializer)
  }

  private def statement(): Statement = {
    if (matchNext(PRINT)) {
      printStatement()
    } else if (matchNext(LEFT_BRACE)) {
      blockStatement()
    }
    else {
      expressionStatement()
    }
  }

  private def blockStatement(): Statement = {
    val statements = ListBuffer.empty[Statement]
    while (!check(RIGHT_BRACE) && !isAtEnd){
      statements += declaration()
    }
    consume(RIGHT_BRACE, "Expect } after block")
    BlockStatement(statements.toList)
  }

  private def printStatement(): Statement = {
    val expr: Expression = expression()
    consume(SEMICOLON, "Expect ; after value.")
    PrintStatement(expr)
  }

  private def expressionStatement(): Statement = {
    val expr: Expression = expression()
    consume(SEMICOLON, "Expect ; after value.")
    ExpressionStatement(expr)
  }

  private def expression(): Expression = assignment()

  private def assignment(): Expression = {
    val expression = equality()
    if (matchNext(EQUAL)){
      val equals: Token = previous()
      val value = assignment()

      if (expression.isInstanceOf[Variable]) {
        val name: Token = expression.asInstanceOf[Variable].name
        return Assign(name, value)
      }
      error(equals, "Invalid assignment target")
    }
    expression
  }

  private def equality(): Expression = {
    var expr: Expression = comparison()
    while(matchNext(BANG_EQUAL, EQUAL_EQUAL)){
      val operator: Token = previous()
      val right: Expression = comparison()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def matchNext(types: TokenType*): Boolean = {
    for t <- types do
      if check(t) then
        advance()
        return true
    false
  }

  private def check(tokenType: TokenType): Boolean = {
    if isAtEnd then false else peek().tokenType == tokenType
  }

  private def advance(): Token = {
    if (!isAtEnd) {
      current += 1
    }
    previous()
  }

  private def isAtEnd: Boolean = peek().tokenType == EOF

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)

  private def comparison(): Expression = {
    var expr: Expression = term()
    while (matchNext(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)){
      val operator: Token = previous()
      val right: Expression = term()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def term(): Expression = {
    var expr: Expression = factor()
    while (matchNext(MINUS, PLUS)) {
      val operator: Token = previous()
      val right: Expression = factor()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def factor(): Expression = {
    var expr: Expression = unary()
    while (matchNext(SLASH, STAR)) {
      val operator: Token = previous()
      val right: Expression = unary()
      expr = Binary(expr, operator, right)
    }
    expr
  }

  private def unary(): Expression = {
    if (matchNext(BANG, MINUS)) {
      val operator: Token = previous()
      val right: Expression = unary()
      return Unary(operator, right)
    }
    primary()
  }

  private def primary(): Expression = {
    if matchNext(FALSE) then Literal(false)
    else if matchNext(TRUE) then Literal(true)
    else if matchNext(NIL) then Literal(null)
    else if matchNext(IDENTIFIER) then Variable(previous())
    else if matchNext(NUMBER, STRING) then Literal(previous().literal)
    else if matchNext(LEFT_PAREN) then
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    else
      throw error(peek(), "Expect expression.")
  }

  private def consume(tokenType: TokenType, message: String): Token = {
    if check(tokenType) then
      advance()
    else
      throw error(peek(), message)
  }

  private def error(token: Token, message: String): ParsingError = {
    Lox.error(token, message)
    ParsingError()
  }

  private def synchronize(): Unit = {
    advance()
    while (!isAtEnd) {
      if (previous().tokenType == SEMICOLON) then return
      peek().tokenType match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => advance()
      }
    }

  }

}
