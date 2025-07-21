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
    false
  }

  private def check(tt: TokenType): Boolean = {
    if (isAtEnd) {
      false
    } else {
      peek().tokenType == tt
    }

  }

  private def advance(): Token = {
    if (!isAtEnd) current += 1
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
      Unary(operator, right)
    }
    primary()
  }

  private def primary(): Expression = {
    if (matchNext(FALSE)) {
      Literal(false)
    }
    if (matchNext(TRUE)){
      Literal(true)
    }
    if (matchNext(NIL)){
      Literal(null)
    }
    if (matchNext(NUMBER, STRING)) {
      Literal(previous().literal)
    }
    if (matchNext(LEFT_PAREN)) {
      val expr: Expression = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    }
    throw error(peek(), "Expect expression.")
  }

  private def consume(tt: TokenType, message: String): Unit = {
    if (check(tt)){
      advance()
    }
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
