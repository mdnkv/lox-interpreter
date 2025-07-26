package dev.mednikov.loxscala

import dev.mednikov.loxscala.errors.RuntimeError
import dev.mednikov.loxscala.scanner.Token

import scala.collection.mutable

class Environment(var enclosing: Environment = null) {

  private val values = mutable.Map.empty[String, Any]


  def define(name: String, value: Any): Unit = values.put(name, value)

  def get(name: Token): Any = {
    try {
      values(name.lexeme)
    } catch {
      case e: NoSuchElementException => {
        if (enclosing != null) {
          enclosing.get(name)
        } else {
          throw RuntimeError(name, s"Undefined variable ${name.lexeme}")
        }
      }
    }
  }

  def assign(name: Token, value: Any): Unit = {
    if (values.contains(name.lexeme)){
      values.put(name.lexeme, value)
    } else {
      if (enclosing != null) {
        enclosing.assign(name, value)
      } else {
        throw RuntimeError(name, s"Undefined variable ${name.lexeme}.")
      }
    }
  }

}
