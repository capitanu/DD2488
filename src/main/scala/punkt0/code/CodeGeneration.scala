package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {

    var varMap: Map[Int, Int] = Map[Int, Int]()

    def mapType(t: Type): String = t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TString => "Ljava/lang/String;"
      case TUnit => "V"
      case TAnyRef(obj) => "L" + obj.name + ";"
      case _ => "fail"
    }

    def recurseExpr(byteCode: CodeHandler, expr: ExprTree, clsSym: ClassSymbol, methSym: MethodSymbol): Unit = expr match {
      case And(lhs, rhs) =>
        val andFalse = byteCode.getFreshLabel("ANDFALSE")
        val andTrue = byteCode.getFreshLabel("ANDTRUE")
        recurseExpr(byteCode, lhs, clsSym, methSym)
        byteCode << IfEq(andFalse)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        byteCode << IfEq(andFalse)
        byteCode << Ldc(1)
        byteCode << Goto(andTrue)
        byteCode << Label(andFalse)
        byteCode << Ldc(0)
        byteCode << Label(andTrue)

      case Or(lhs, rhs) =>
        val orFalse = byteCode.getFreshLabel("ORFALSE")
        val orTrue = byteCode.getFreshLabel("ORTRUE")
        recurseExpr(byteCode, lhs, clsSym, methSym)
        byteCode << IfNe(orTrue)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        byteCode << IfNe(orTrue)
        byteCode << Ldc(0)
        byteCode << Goto(orFalse)
        byteCode << Label(orTrue)
        byteCode << Ldc(1)
        byteCode << Label(orFalse)

      case Plus(lhs, rhs) =>
        if(lhs.getType == TInt && rhs.getType == TInt) {
          recurseExpr(byteCode, lhs, clsSym, methSym)
          recurseExpr(byteCode, rhs, clsSym, methSym)
          byteCode << IADD
        } else {
          byteCode << DefaultNew("java/lang/StringBuilder")
          recurseExpr(byteCode, lhs, clsSym, methSym)
          byteCode << InvokeVirtual("java/lang/StringBuilder", "append", "(" + mapType(lhs.getType) + ")Ljava/lang/StringBuilder;")
          recurseExpr(byteCode, rhs, clsSym, methSym)
          byteCode << InvokeVirtual("java/lang/StringBuilder", "append", "(" + mapType(rhs.getType) + ")Ljava/lang/StringBuilder;")
          byteCode << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

        }
      case Minus(lhs, rhs) =>
        recurseExpr(byteCode, lhs, clsSym, methSym)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        byteCode << ISUB
      case Times(lhs, rhs) =>
        recurseExpr(byteCode, lhs, clsSym, methSym)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        byteCode << IMUL
      case Div(lhs, rhs) =>
        recurseExpr(byteCode, lhs, clsSym, methSym)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        byteCode << IDIV
      case LessThan(lhs, rhs) =>
        val eqTrue = byteCode.getFreshLabel("LTTRUE")
        val eqFalse = byteCode.getFreshLabel("LTFALSE")
        recurseExpr(byteCode, lhs, clsSym, methSym)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        byteCode << If_ICmpLt(eqTrue)
        byteCode << Ldc(0)
        byteCode << Goto(eqFalse)
        byteCode << Label(eqTrue)
        byteCode << Ldc(1)
        byteCode << Label(eqFalse)
      case Equals(lhs, rhs) =>
        val eqTrue = byteCode.getFreshLabel("EQTRUE")
        val eqFalse = byteCode.getFreshLabel("EQFALSE")
        recurseExpr(byteCode, lhs, clsSym, methSym)
        recurseExpr(byteCode, rhs, clsSym, methSym)
        if((lhs.getType == TInt && rhs.getType == TInt) || (rhs.getType == TBoolean && lhs.getType == TBoolean))
          byteCode << If_ICmpEq(eqTrue)
          else
            byteCode << If_ACmpEq(eqTrue)
        byteCode << Ldc(0)
        byteCode << Goto(eqFalse)
        byteCode << Label(eqTrue)
        byteCode << Ldc(1)
        byteCode << Label(eqFalse)

      case MethodCall(obj, meth, args) =>
        recurseExpr(byteCode, obj, clsSym, methSym)
        var sign = "("
        args.foreach(a => {
          recurseExpr(byteCode, a, clsSym, methSym)
          sign = sign + mapType(a.getType)
        })
        sign = sign + ")"
        sign = sign + mapType(meth.getType)
        byteCode << InvokeVirtual(obj.getType.asInstanceOf[TAnyRef].classSymbol.name, meth.value, sign)
      case IntLit(value) =>
        byteCode << Ldc(value)
      case StringLit(value) =>
        byteCode << Ldc(value)
      case True() =>
        byteCode << Ldc(1)
      case False() =>
        byteCode << Ldc(0)
      case x @ Identifier(value) =>
        if(varMap.get(x.getSymbol.asInstanceOf[VariableSymbol].id) != None){
          x.getSymbol.getType match {
            case TInt =>
              byteCode << ILoad(varMap.get(x.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case TBoolean =>
              byteCode << ILoad(varMap.get(x.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case TString =>
              byteCode << ALoad(varMap.get(x.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case TAnyRef(_) =>
              byteCode << ALoad(varMap.get(x.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case _ =>
          }
        } else if(methSym.argList.contains(x.getSymbol.asInstanceOf[VariableSymbol])) {
          byteCode << ArgLoad(methSym.argList.indexOf(x.getSymbol.asInstanceOf[VariableSymbol]) + 1)
        } else if(clsSym.members.contains(x.getSymbol.asInstanceOf[VariableSymbol].name)) {
          byteCode << ArgLoad(0)
          byteCode << GetField(clsSym.name, x.getSymbol.asInstanceOf[VariableSymbol].name, mapType(x.getSymbol.getType))
        }

      case This() =>
        byteCode << ArgLoad(0)
      case Null() =>
        byteCode << ACONST_NULL
      case New(tpe) =>
        byteCode << DefaultNew(tpe.getSymbol.asInstanceOf[ClassSymbol].name)
      case Not(expr) =>
        val noInverse = byteCode.getFreshLabel("NOINV")
        val afterInverse = byteCode.getFreshLabel("AFTINV")
        recurseExpr(byteCode, expr, clsSym, methSym)
        byteCode << IfEq(noInverse)
        byteCode << Ldc(0)
        byteCode << Goto(afterInverse)
        byteCode << Label(noInverse)
        byteCode << Ldc(1)
        byteCode << Label(afterInverse)
      case Block(exprs) =>
        exprs.foreach(e => {
          recurseExpr(byteCode, e, clsSym, methSym)
        })
      case If(expr, thn, els) =>
        recurseExpr(byteCode, expr, clsSym, methSym)
        els match {
          case Some(elsBranch) =>
            val labelFalse = byteCode.getFreshLabel("IFFALSE")
            val labelFI = byteCode.getFreshLabel("FI")
            byteCode << IfEq(labelFalse)
            recurseExpr(byteCode, thn, clsSym, methSym)
            byteCode << Goto(labelFI)
            byteCode << Label(labelFalse)
            recurseExpr(byteCode, elsBranch, clsSym, methSym)
            byteCode << Label(labelFI)
          case None =>
            val labelFI = byteCode.getFreshLabel("FI")
            byteCode << IfEq(labelFI)
            recurseExpr(byteCode, thn, clsSym, methSym)
            byteCode << Label(labelFI)
        }
      case While(cond, body) =>
        val labelBeforeWhile = byteCode.getFreshLabel("BFRWHILE")
        val labelAfterWhile = byteCode.getFreshLabel("AFTWHILE")
        byteCode << Label(labelBeforeWhile)
        recurseExpr(byteCode, cond, clsSym, methSym)
        byteCode << IfEq(labelAfterWhile)
        recurseExpr(byteCode, body, clsSym, methSym)
        byteCode << Goto(labelBeforeWhile)
        byteCode << Label(labelAfterWhile)
      case Println(e) =>
        byteCode << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        recurseExpr(byteCode, e, clsSym, methSym)
        byteCode << InvokeVirtual("java/io/PrintStream", "println", "(" + mapType(e.getType) + ")V")
      case Assign(id, expr) =>
        if(varMap.get(id.getSymbol.asInstanceOf[VariableSymbol].id) == None) {

          byteCode << ArgLoad(0)
          recurseExpr(byteCode, expr, clsSym, methSym)
          byteCode << PutField(clsSym.name, id.value, mapType(id.getType))

        } else {
          recurseExpr(byteCode, expr, clsSym, methSym)

          id.getSymbol.getType match {
            case TInt =>
              byteCode << IStore(varMap.get(id.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case TBoolean =>
              byteCode << IStore(varMap.get(id.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case TString =>
              byteCode << AStore(varMap.get(id.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case TAnyRef(_) =>
              byteCode << AStore(varMap.get(id.getSymbol.asInstanceOf[VariableSymbol].id).get)
            case _ =>
          }
        }

    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      var par: Option[String] = None
      ct.parent match {
        case Some(p) => par = Some(p.value)
        case None => par = None
      }
      val classFile = new ClassFile(ct.id.value, par)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      ct.vars.foreach(v => {
        classFile.addField(mapType(v.getSymbol.getType), v.id.value)
      })

      ct.methods.foreach(m => {
        var args: String = ""
        m.args.foreach(x => args = args + mapType(x.getSymbol.getType))
        val mh: MethodHandler = classFile.addMethod(mapType(m.getSymbol.getType), m.id.value, args)
        generateMethodCode(mh.codeHandler, m)
      })

      classFile.writeToFile(dir + ct.id.value + ".class")
    }

    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      mt.vars.foreach(v => {

        recurseExpr(ch, v.expr, methSym.classSymbol, methSym)

        varMap = varMap + (v.getSymbol.id -> ch.getFreshVar)
        v.getSymbol.getType match {
          case TInt =>
            ch << IStore(varMap.get(v.getSymbol.id).get)
          case TBoolean =>
            ch << IStore(varMap.get(v.getSymbol.id).get)
          case TString =>
            ch << AStore(varMap.get(v.getSymbol.id).get)
          case TAnyRef(_) =>
            ch << AStore(varMap.get(v.getSymbol.id).get)
          case _ =>
        }
      })

      mt.exprs.foreach(e => {
        recurseExpr(ch, e, methSym.classSymbol, methSym)
        if(e.getType != TUnit)
          ch << POP
      })
      recurseExpr(ch, mt.retExpr, methSym.classSymbol, methSym)

      mt.getSymbol.getType match {
        case TInt =>
          ch << IRETURN
        case TBoolean =>
          ch << IRETURN
        case TString =>
          ch << ARETURN
        case TAnyRef(_) =>
          ch << ARETURN
        case TUnit =>
          ch << RETURN
        case _ =>
      }
      ch.freeze
    }

    def generateMainFile(outdir: String): Unit = {
      val mainFile = new ClassFile(prog.main.obj.value, None)
      mainFile.addDefaultConstructor
      val main = mainFile.addMainMethod.codeHandler
      prog.main.vars.foreach(v => {
        recurseExpr(main, v.expr, prog.main.getSymbol, null)
        varMap = varMap + (v.getSymbol.id -> main.getFreshVar)
        v.getSymbol.getType match {
          case TInt =>
            main << IStore(varMap.get(v.getSymbol.id).get)
          case TBoolean =>
            main << IStore(varMap.get(v.getSymbol.id).get)
          case TString =>
            main << AStore(varMap.get(v.getSymbol.id).get)
          case TAnyRef(_) =>
            main << AStore(varMap.get(v.getSymbol.id).get)
          case _ =>
        }
      })

      prog.main.exprs.foreach(e => {
        recurseExpr(main, e, prog.main.getSymbol, null)
        if(e.getType != TUnit)
          main << POP
      })

      main << RETURN
      main.freeze
      mainFile.writeToFile(outdir + prog.main.obj.value + ".class")
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    generateMainFile(outDir)
  }

}
