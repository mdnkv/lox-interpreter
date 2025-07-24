package dev.mednikov.loxscala

import dev.mednikov.loxscala.errors.RuntimeError
import dev.mednikov.loxscala.interpreter.Interpreter
import dev.mednikov.loxscala.parser.Parser
import dev.mednikov.loxscala.scanner.TokenType.EOF
import dev.mednikov.loxscala.scanner.{Scanner, Token}

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox {

  private val interpreter = Interpreter
  private var hadError: Boolean = false
  private var hadRuntimeError: Boolean = false

  private def run (payload: String): Unit = {
    val scanner = Scanner(payload)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    val expression = parser.parse()
    if (hadError) then return
    interpreter.interpret(expression)
  }

  private def runFile(path: String): Unit = {
    val bytes: Array[Byte] = Files.readAllBytes(Paths.get(path))
    run(String(bytes, Charset.defaultCharset()))
    if (hadError) System.exit(65)
    if (hadRuntimeError) System.exit(70)
  }

  private def runPrompt(): Unit = {
    val input: InputStreamReader = InputStreamReader(System.in)
    val reader: BufferedReader = BufferedReader(input)
    while true do {
      print("> ")
      var line = reader.readLine()
      if line == null || line.trim == "exit" then return
      run(line)
      hadError = false
    }
  }
  
  def error (lineNumber: Int, message: String): Unit = {
    println(s"[line: $lineNumber] Error: $message")
    hadError = true
  }

  def error(token: Token, message: String): Unit = {
    if (token.tokenType == EOF) {
      error(token.lineNumber, s" at end, $message")
    } else {
      val lexeme = token.lexeme
      error(token.lineNumber, s" at '$lexeme' $message")
    }
  }

  def runtimeError(e: RuntimeError): Unit = {
    val message = e.message
    val lineNumber = e.token.lineNumber
    println(s"$message \n[Line $lineNumber]")
    hadRuntimeError = true
  }

  def main(args: Array[String]): Unit = {
    if args.length > 1 then
      println("Usage: lox <script name>")
      sys.exit(64)
    else if args.length == 1 then
      runFile(args(0))
    else
      runPrompt()
  }

}
