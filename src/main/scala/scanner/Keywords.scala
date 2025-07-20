package dev.mednikov.loxscala
package scanner

import scanner.TokenType._

val keywords: Map[String, TokenType] = Map(
  "and" -> AND,
  "class" -> CLASS,
  "else" -> ELSE,
  "false" -> FALSE,
  "for" -> FOR,
  "fun" -> FUN,
  "IF" -> IF,
  "nil" -> NIL,
  "or" -> OR,
  "print" -> PRINT,
  "return" -> RETURN,
  "super" -> SUPER,
  "this" -> THIS,
  "true" -> TRUE,
  "var" -> VAR,
  "while" -> WHILE
)