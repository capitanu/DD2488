package punkt0

import java.io.File

import lexer._
import ast._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()
    var token = false

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
        processOption(args)

      case "--print" :: args =>
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)

      case "--symid" :: args =>
        ctx = ctx.copy(doSymbolIds = true)
        processOption(args)
      case "--ast+" :: args =>
        ctx = ctx.copy(doASTPlus = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help         displays this help")
    println(" --tokens       print all tokens")
    println(" --ast          creates the AST")
    println(" --print        prints the AST")
    println(" --symid        adds the symbold ids")
    println(" --ast+         adds the symbold ids")
    println(" -d <outdir>    generates class files in the specified directory")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    val lexer = Lexer.run(ctx.file.get)(ctx)
    val ast = Parser.run(lexer)(ctx)
    val symIDs = NameAnalysis.run(ast)(ctx)
    val ppSymIds = Printer.apply(symIDs)
    val pp = Printer.apply(ast)
    val typeChecking = TypeChecking.run(symIDs)(ctx)
    val astPlus = TypedASTPrinter.apply(typeChecking)
    //CodeGeneration.run(typeChecking)(ctx)

    if(ctx.doTokens)
      while(lexer.hasNext)
        println(lexer.next())

    if(ctx.doAST)
      println(ast)

    if(ctx.doPrintMain)
      println(pp)

    if(ctx.doSymbolIds)
      println(ppSymIds)

    if(ctx.doASTPlus)
      println(astPlus)

    Reporter.terminateIfErrors()
  }

}
