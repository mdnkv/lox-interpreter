package dev.mednikov.loxscala
package scanner

case class Token(tokenType: TokenType, lexeme: String, literal: Option[Any], lineNumber: Int):
  override def toString: String = {
    s"$tokenType $lexeme $literal $lineNumber"
  }
