package dev.mednikov.loxscala
package scanner

import dev.mednikov.loxscala.scanner.TokenType._

import scala.collection.mutable.ListBuffer

class Scanner (source: String) {

  private var start: Int = 0
  private var current: Int = 0
  private var lineNumber: Int = 1

  private val tokens = ListBuffer.empty[Token]

  private def isAtEnd: Boolean = current >= source.length

  private def scanToken(): Unit = {
    val ch = advance()
    ch match
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => if (matchNext('=')) addToken(BANG_EQUAL) else addToken(BANG)
      case '=' => if (matchNext('=')) addToken(EQUAL_EQUAL) else addToken(EQUAL)
      case '<' => if (matchNext('=')) addToken(LESS_EQUAL) else addToken(LESS)
      case '>' => if (matchNext('=')) addToken(GREATER_EQUAL) else addToken(GREATER)
      case '/' => if (matchNext('/')) {
        while(peek() != '\n' && isAtEnd) advance()
      } else {
        addToken(SLASH)
      }
      case ' ' => ()
      case '\r' => ()
      case '\t' => ()
      case '\n' => lineNumber += 1
      case '"' => str()
      case _ => {
        if (isDigit(ch)) number()
        else if (isAlpha(ch)) identifier()
        else Lox.error(lineNumber, s"Unexpected character: $ch")
      }
  }

  private def isDigit(input: Char): Boolean = input >= '0' && input <= '9'

  private def isAlpha(input: Char): Boolean = (input >= 'a' && input <= 'z') ||
    (input >= 'A' && input <= 'Z') ||
    input == '_'

  private def isAlphaNum (input: Char) = isAlpha(input) || isDigit(input)

  private def identifier(): Unit = {
    while (isAlphaNum(peek())) advance()
    val str = source.substring(start, current)
    val tokenType = keywords.getOrElse(str, IDENTIFIER)
    addToken(tokenType)
  }

  private def number (): Unit = {
    while (isDigit(peek())) advance()
    if (peek() == '.' && isDigit(peekNext())){
      advance()
      while(isDigit(peek())) advance()
    }
    val x: Double = source.substring(start, current).toDouble
    addToken(NUMBER, Some(x))
  }

  private def str(): Unit = {
    while (peek() != '"' && !isAtEnd){
      if (peek() == '\n') lineNumber += 1
      advance()
    }

    if (isAtEnd){
      println(s"[line number $lineNumber Unterminated string.")
      return
    }

    advance()

    val str = source.substring(start + 1, current - 1)
    addToken(STRING, Some(str))

  }

  private def peek(): Char = {
    if (isAtEnd) '\u0000' else source.charAt(current)
  }

  private def peekNext(): Char = {
    if (current + 1 >= source.length) return '\u0000'
    source.charAt(current + 1)
  }

  private def matchNext (expected: Char): Boolean = {
    if (isAtEnd || source.charAt(current) != expected) false
    else {
      current += 1
      true
    }
  }


  private def advance(): Char = {
    val ch = source.charAt(current)
    current += 1
    ch
  }

  private def addToken (tokenType: TokenType, literal: Option[Any] = None): Unit = {
    val text = source.substring(start, current)
    tokens += Token(tokenType, text, literal, lineNumber)
  }

  def scanTokens(): List[Token] = {
    while (!isAtEnd) {
      start = current
      scanToken()
    }
    tokens += Token(EOF, "", None, lineNumber)
    tokens.toList
  }

}
