package dev.mednikov.loxscala
package interpreter

case class Token(tokenType: TokenType, lexeme: String, literal: Option[Any], lineNumber: Int):
  override def toString: String = {
    s"$tokenType $lexeme $literal $lineNumber"
  }
