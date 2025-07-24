package dev.mednikov.loxscala
package interpreter

import parser.*

import dev.mednikov.loxscala.errors.RuntimeError
import dev.mednikov.loxscala.scanner.Token
import dev.mednikov.loxscala.scanner.TokenType.*

object Interpreter {

  def evaluation(expr: Expression): Any = {
    expr match {
      case Literal(Some(v)) => v
      case Literal(None) => null
      case Grouping(expression) => evaluation(expression)
      case Unary(operator, right) => {
        val rightVal = evaluation(right)
        operator.tokenType match {
          case MINUS => {
            checkNumberOperand(operator, right)
            -rightVal.asInstanceOf[Double]
          }
          case BANG => !isTruthy(rightVal)
          case _ => null
        }
      }
      case Binary(left, operator, right) => {
        val leftVal = evaluation(left)
        val rightVal = evaluation(right)
        operator.tokenType match {
          case MINUS => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] - rightVal.asInstanceOf[Double]
          }
          case SLASH => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] / rightVal.asInstanceOf[Double]
          }
          case STAR => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] * rightVal.asInstanceOf[Double]
          }
          case PLUS => {
            (leftVal, rightVal) match {
              case (num1: Double, num2: Double) => num1 + num2
              case (str1: String, str2: String) => str1 + str2
              case _ => throw RuntimeError(operator, "Operands must be 2 numbers or 2 strings")
            }
          }
          case GREATER => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] > rightVal.asInstanceOf[Double]
          }
          case GREATER_EQUAL => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] >= rightVal.asInstanceOf[Double]
          }
          case LESS => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] < rightVal.asInstanceOf[Double]
          }
          case LESS_EQUAL => {
            checkNumberOperands(operator, leftVal, rightVal)
            leftVal.asInstanceOf[Double] <= rightVal.asInstanceOf[Double]
          }
          case BANG_EQUAL => !isEqual(leftVal, rightVal)
          case EQUAL_EQUAL => isEqual(leftVal, rightVal)
          case _ => null
        }
      }
      case _ => null
    }
  }

  def interpret (expr: Expression): Unit = {
    try {
      val value = evaluation(expr)
      println(stringify(value))
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }
  }

  private def isTruthy(value: Any): Boolean = {
    value match {
      case null => false
      case b: Boolean => b
      case _ => true
    }
  }

  private def isEqual(a: Any, b: Any): Boolean = {
    if a == null && b == null then true
    else if a == null then false
    else a == b
  }

  private def checkNumberOperand (operator: Token, operand: Any) : Unit = {
    if (operand.isInstanceOf[Double]) return
    throw RuntimeError(operator, "Operand must be a number")
  }

  private def checkNumberOperands (operator: Token, left: Any, right: Any): Unit = {
    if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) return
    throw RuntimeError(operator, "Operand must be a numbers")
  }

  private def stringify(value: Any): String = {
    value match
      case null => "nil"
      case d: Double => {
        val text = value.toString
        if (text.endsWith(".0")){
          return text.substring(0, text.length - 2)
        }
        text
      }
      case _ => value.toString
  }

}
