package punkt0
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    def recursiveApply(tree: Tree): String = {

      tree match {
        case Program(main, classes) =>
          var rtn = ""
          classes.foreach(clas => rtn = rtn + recursiveApply(clas) + "\n")
          rtn = rtn + recursiveApply(main)
          rtn
        case MainDecl(obj, parent, vars, expr) =>
          var rtn = "object " + recursiveApply(obj) + " extends " + recursiveApply(parent) + " {\n"
          vars.foreach(v => rtn = rtn + recursiveApply(v))
          expr.foreach(e => rtn = rtn + recursiveApply(e) + "; \n")
          rtn = rtn + "}\n"
          rtn
        case ClassDecl(id, parent, vars, method) =>
          var rtn = "class " + recursiveApply(id)
          parent match {
            case Some(x) => rtn = rtn + " extends " + recursiveApply(x) + " {\n"
            case _ => rtn = rtn + " {\n"
          }
          vars.foreach(v => rtn = rtn + recursiveApply(v))
          method.foreach(e => rtn = rtn + recursiveApply(e))
          rtn + "}\n"
        case VarDecl(tpe, id, expr) =>
          "var " + id.value.toString + ": " + recursiveApply(tpe) + " = " + recursiveApply(expr) + ";\n"
        case MethodDecl(overrides, retType, id, args, vars, expr, retExpr) =>
          var rtn = ""
          if(overrides)
            rtn = rtn + "override def "
          else
            rtn = rtn + "def "
          rtn = rtn + id.value + "("
          var lastArg = args.last
          var argsTemp = args
          argsTemp = argsTemp.dropRight(1)
          argsTemp.foreach(arg => rtn = rtn + recursiveApply(arg) +  ", ")
          rtn = rtn + recursiveApply(lastArg) + "): " + recursiveApply(retType) + " = {\n"
          vars.foreach(v => rtn = rtn + recursiveApply(v))
          expr.foreach(e => rtn = rtn + recursiveApply(e) + "; \n")
          rtn = rtn + recursiveApply(retExpr)
          rtn + "}\n"
        case BooleanType() => "Boolean"
        case Formal(tpe, id) =>
          id.value.toString + ": " + recursiveApply(tpe)
        case IntType() => "Int"
        case StringType() => "String"
        case UnitType() => "Unit"
        case And(lhs, rhs) => recursiveApply(lhs) + " && " + recursiveApply(rhs)
        case Or(lhs, rhs) => recursiveApply(lhs) + " || " + recursiveApply(rhs)
        case Minus(lhs, rhs) => recursiveApply(lhs) + " - " + recursiveApply(rhs)
        case Times(lhs, rhs) => recursiveApply(lhs) + " * " + recursiveApply(rhs)
        case Div(lhs, rhs) => recursiveApply(lhs) + " / " + recursiveApply(rhs)
        case LessThan(lhs, rhs) => recursiveApply(lhs) + " < " + recursiveApply(rhs)
        case Equals(lhs, rhs) => recursiveApply(lhs) + " == " + recursiveApply(rhs)
        case MethodCall(obj, meth, args) =>
          var rtn = recursiveApply(obj) + "." + recursiveApply(meth) + "("
          var lastArg = args.last
          var argsTemp = args
          argsTemp = argsTemp.dropRight(1)
          argsTemp.foreach(arg => rtn = rtn + recursiveApply(arg) +  ", ")
          rtn + recursiveApply(lastArg) + ")"
        case IntLit(value) => " " + value.toString + " "
        case StringLit(value) => " " + value + " "
        case True() => " true "
        case False() => " false "
        case Identifier(value) => " " + value + " "
        case This() => " this"
        case Null() => " null "
        case New(tpe) => "new " + recursiveApply(tpe) + "()"
        case Not(expr) => "!" + recursiveApply(expr)
        case Block(expr) =>
          var rtn = "{\n"
          expr.foreach(exp => rtn = rtn + recursiveApply(exp) + "; \n")
          rtn = rtn + "}\n"
          rtn
        case If(expr, thn, els) =>
          els match {
            case Some(x) =>
              "if(" + recursiveApply(expr) + ") " + recursiveApply(thn) + " else " + recursiveApply(x)
            case _ =>
            "if(" + recursiveApply(expr) + ") " + recursiveApply(thn)
          }
        case While(cond, body) =>
          "while(" + recursiveApply(cond) + ")" + recursiveApply(body) + "\n"
        case Println(expr) =>
          "println(" + recursiveApply(expr) + ")"
        case Assign(id, expr) =>
          id.value.toString + " = " + recursiveApply(expr)
        case _ =>
          ""
      }

    }

    recursiveApply(t)
  }
}

