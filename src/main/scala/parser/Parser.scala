package dev.mednikov.loxscala
package parser

import scanner.{Token, TokenType}

import dev.mednikov.loxscala.scanner.TokenType.*


class Parser(tokens: List[Token]) {

  private var current: Int = 0

  def parse(): Expression = {
    try {
      expression()
    } catch {
      case e: ParsingError => null
    }
  }

  private def expression(): Expression = equality()

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
    if matchNext(TokenType.FALSE) then Literal(false)
    else if matchNext(TokenType.TRUE) then Literal(true)
    else if matchNext(TokenType.NIL) then Literal(null)
    else if matchNext(TokenType.NUMBER, TokenType.STRING) then Literal(previous().literal)
    else if matchNext(TokenType.LEFT_PAREN) then
      val expr = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
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
