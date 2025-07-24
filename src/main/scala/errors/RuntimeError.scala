package dev.mednikov.loxscala
package errors

import scanner.Token

case class RuntimeError(token: Token, message: String) extends RuntimeException