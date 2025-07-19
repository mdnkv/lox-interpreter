package dev.mednikov.loxscala

import dev.mednikov.loxscala.interpreter.Scanner

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox {

  private var hadError: Boolean = false

  private def run (payload: String): Unit = {
    val scanner = Scanner(payload)
    val tokens = scanner.scanTokens()
    tokens.foreach(println)
  }

  private def runFile(path: String): Unit = {
    val bytes: Array[Byte] = Files.readAllBytes(Paths.get(path))
    run(String(bytes, Charset.defaultCharset()))
    if (hadError) System.exit(65)
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
