package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    val global = new GlobalScope

    def recurseExpr(expr: ExprTree, sym: Symbol): Unit = {
      expr match {
        case And(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case Or(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case Plus(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case Minus(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case Times(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case Div(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case LessThan(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case Equals(lhs, rhs) =>
          recurseExpr(lhs, sym)
          recurseExpr(rhs, sym)
        case MethodCall(obj, meth, args) =>
        // Fix method call
        case IntLit(_) =>
        case StringLit(_) =>
        case True() =>
        case False() =>
        case Identifier(value) =>
          sym match {
            case _:ClassSymbol =>
              sym.asInstanceOf[ClassSymbol].lookupVar(value) match {
                case Some(v) =>
                  expr.asInstanceOf[Identifier].setSymbol(v)
                case None =>
                  sym.asInstanceOf[ClassSymbol].lookupMethod(value) match {
                    case Some(m) =>
                      expr.asInstanceOf[Identifier].setSymbol(m)
                    case None =>
                      global.lookupClass(value) match {
                        case Some(c) =>
                          expr.asInstanceOf[Identifier].setSymbol(c)
                        case None =>
                          sys.error("Identifier not declared in current class")
                      }
                  }
              }
            case _:MethodSymbol =>
              sym.asInstanceOf[MethodSymbol].lookupVar(value) match {
                case Some(v) =>
                  expr.asInstanceOf[Identifier].setSymbol(v)
                case None =>
                  global.lookupClass(value) match {
                    case Some(c) =>
                      expr.asInstanceOf[Identifier].setSymbol(c)
                    case None =>
                      sys.error("Identifier not declared in the current method")
                  }
              }
            case _ => sys.error("Identifier must be either in ClassSymbol or MethodSymbol")
          }
        case This() =>
          expr.asInstanceOf[This].setSymbol(sym.asInstanceOf[ClassSymbol])
        case Null() =>
        case New(tpe) =>
          global.lookupClass(tpe.value) match {
            case Some(cls) => tpe.setSymbol(cls)
            case None => sys.error("Class is not declared")
          }
        case Not(expr) =>
          recurseExpr(expr, sym)
        case Block(exprs) =>
          exprs.foreach(exp => {
            recurseExpr(exp, sym)
          })
        case If(expr, thn, els) =>
          recurseExpr(expr, sym)
          recurseExpr(thn, sym)
          els match {
            case Some(x) => recurseExpr(x, sym)
            case None => ()
          }
        case While(cond, body) =>
          recurseExpr(cond, sym)
          recurseExpr(body, sym)
        case Println(expr) =>
          recurseExpr(expr, sym)
        case Assign(id, expr) =>
          if(sym.asInstanceOf[MethodSymbol].params.contains(id.value))
            sys.error("Method argument already defined")
          recurseExpr(id, sym)
          recurseExpr(expr, sym)
        case _ => sys.error("Unrecognized expression")
      }
    }


    prog.classes.foreach(cls => {
      val sym = (new ClassSymbol(cls.id.value)).setPos(cls)
      cls.setSymbol(sym)
      cls.id.setSymbol(sym)
      if(global.lookupClass(cls.id.value) == None) {
        global.classes += (cls.id.value -> sym)
      } else {
        sys.error("Class already in global scope")
      }
    })

    prog.classes.foreach(cls => {
      if(cls.parent != None) {
        var parentSym = global.lookupClass(cls.parent.getOrElse(sys.error("Class parent not found")).value)
        parentSym match {
          case Some(x) =>
            cls.parent.getOrElse(sys.error("Class parent not found")).setSymbol(x)
            cls.getSymbol.parent = Some(x)
          case None =>
            sys.error("Parent class not found")
        }
        var parSym = cls.getSymbol.getParentSym
        while(parSym != null) {
          if(parSym == cls.getSymbol)
            sys.error("Found cycle in the class graph")
          else
            parSym = parSym.getParentSym
        }
      }
    })

    prog.classes.foreach(cls => {
      cls.vars.foreach(v => {
        val sym = (new VariableSymbol(v.id.value)).setPos(v)
        v.setSymbol(sym)
        v.id.setSymbol(sym)
        if(cls.getSymbol.lookupVar(v.id.value) == None){
          v.expr match {
            case IntLit(_) | StringLit(_) | New(_) | True() | False() | Null() =>
              cls.getSymbol.members += (v.getSymbol.name -> v.getSymbol)
              recurseExpr(v.expr, cls.getSymbol)
            case _ => sys.error("Wrong variable expression")
          }
        } else {
          sys.error("Variable already declared in class scope")
        }
      })

      cls.methods.foreach(m => {
        val sym = (new MethodSymbol(m.id.value, cls.getSymbol)).setPos(m)
        m.setSymbol(sym)
        m.id.setSymbol(sym)
        if(m.overrides == true && cls.parent == None)
          sys.error("Method can not override if there is not parent class")
        if(m.overrides == false || (m.overrides == true && cls.parent != None)) {
          cls.getSymbol.lookupMethod(m.id.value) match {
            case Some(x) => sys.error("Method already exists in current class scope")
            case None => cls.getSymbol.methods = cls.getSymbol.methods + (m.id.value -> sym)
          }
          cls.parent match {
            case Some(par) => par.getSymbol.asInstanceOf[ClassSymbol].lookupMethod(m.id.value) match {
              case Some(x) => sys.error("Method already exists in current class scope")
              case None =>
                cls.getSymbol.methods = cls.getSymbol.methods + (m.id.value -> sym)
            }
            case None => ()
          }
        }

        m.getSymbol.argList = List[VariableSymbol]()
        m.args.foreach(arg => {
          val sym = (new VariableSymbol(arg.id.value)).setPos(arg)
          arg.setSymbol(sym)
          arg.id.setSymbol(sym)
          if(!m.getSymbol.params.contains(arg.id.value)) {
            m.getSymbol.params = m.getSymbol.params + (arg.id.value -> sym)
            m.getSymbol.argList = m.getSymbol.argList :+ sym
          } else {
            sys.error("Arg already declared in the scope of method")
          }
        })

        m.vars.foreach(v => {
          val sym = (new VariableSymbol(v.id.value)).setPos(v)
          v.setSymbol(sym)
          v.id.setSymbol(sym)

          if(m.overrides) {
            var par = cls.getSymbol.parent
            while(par != None) {
              par.get.lookupVar(v.id.value) match {
                case Some(vprime) => sys.error("Variable already declared in parent class")
                case None => par = par.get.parent
              }
            }

            v.expr match {
              case IntLit(_) | StringLit(_) | New(_) | True() | False() | Null() =>
                recurseExpr(v.expr, cls.getSymbol)
                m.getSymbol.members = m.getSymbol.members + (v.id.value -> sym)
              case _ => sys.error("Wrong variable expression")
            }
          } else {
            if(m.getSymbol.members.contains(v.id.value)) {
              sys.error("Var already declared in method")
            } else {
              v.expr match {
                case IntLit(_) | StringLit(_) | New(_) | True() | False() | Null() =>
                  recurseExpr(v.expr, cls.getSymbol)
                  m.getSymbol.members = m.getSymbol.members + (v.id.value -> sym)
                case _ => sys.error("Wrong variable expression")
              }
            }
          }

        })

      })
    })

    prog.classes.foreach(cls => {
      cls.methods.foreach(m => {
        if(m.overrides && cls.getSymbol.parent == None)
          sys.error("Method overrides but parent is not defined")
        if(m.overrides) {
          cls.getSymbol.parent.get.lookupMethod(m.id.value) match {
            case x @ Some(overriddenMethod) =>
              // Should probably check each parameter?
              m.getSymbol.overridden = x
            case None => sys.error("No overriden method, however declared as overrides.")
          }
        }
      })
      cls.methods.foreach(m => {
        m.exprs.foreach(e => {
          recurseExpr(e, m.getSymbol)
        })
        recurseExpr(m.retExpr, m.getSymbol)
      })
    })


    val mainSym = (new ClassSymbol(prog.main.obj.value)).setPos(prog.main)
    global.mainClass = mainSym
    prog.main.setSymbol(mainSym)
    prog.main.obj.setSymbol(mainSym)
    prog.main.vars.foreach(v => {
      val sym = (new VariableSymbol(v.id.value)).setPos(v)
      v.setSymbol(sym)
      v.id.setSymbol(sym)
      if(global.mainClass.lookupVar(v.id.value) != None) {
        sys.error("Variable already declared in the main class scope.")
      }
      v.expr match {
        case IntLit(_) | StringLit(_) | New(_) | True() | False() | Null() =>
          recurseExpr(v.expr, global.mainClass)
          global.mainClass.members = global.mainClass.members + (v.id.value -> sym)
        case _ => sys.error("Wrong variable expression")
      }
    })
    prog.main.exprs.foreach(e => {
      recurseExpr(e, global.mainClass)
    })

    prog
  }

}
