package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    val global = new GlobalScope

    def getType(tpe: TypeTree): Type = tpe match {
      case IntType() => TInt
      case BooleanType() => TBoolean
      case StringType() => TString
      case UnitType() => TUnit
      case Identifier(v) =>
        var cls = global.lookupClass(v).getOrElse(sys.error("Type does not exist"))
        tpe.asInstanceOf[Identifier].setSymbol(cls)
        cls.getType
      case _ => sys.error("Type does not exist")
    }

    def recurseMethCall(expr: ExprTree, sym: Symbol): TypeTree = {
      expr match {
        case MethodCall(obj, meth, args) =>
          recurseExpr(obj, sym)
          obj match {
            case x @ MethodCall(obj2, meth2, args2) =>
              var y = recurseMethCall(x,sym)
              global.lookupClass(y.getType.toString) match {
                case Some(classFound) =>
                  classFound.lookupMethod(meth.value) match {
                    case Some(methodFound) =>
                      meth.setSymbol(methodFound)
                    case None =>
                      sys.error("Method not found")
                  }
                case None =>
                  sys.error("Class not found")
              }
            case This() =>
              sym match {
                case x: MethodSymbol =>
                  x.classSymbol.lookupMethod(meth.value) match {
                    case Some(m) =>
                      if(m.argList.length == args.length) {
                        meth.setSymbol(m)
                        meth
                      }
                      else
                        sys.error("Wrong number of arguments")
                    case None => sys.error("Method not declared")
                  }
                case x: ClassSymbol =>
                  x.lookupMethod(meth.value) match {
                    case Some(m) =>
                      if(m.argList.length == args.length) {
                        meth.setSymbol(m)
                        meth
                      }
                      else
                        sys.error("Wrong number of arguments")
                    case None => sys.error("Method not declared")
                  }
                case _ => sys.error("Variable not declared in the current scope")
              }
              
            case Identifier(value) => {
              sym match {
                case x: MethodSymbol =>
                  x.lookupVar(value) match {
                    case Some(v) =>
                      obj.asInstanceOf[Identifier].setSymbol(v)
                      val classExists = global.lookupClass(v.getType.toString).getOrElse(sys.error("Class is not defined"))
                      val methodExists = classExists.lookupMethod(meth.value).getOrElse(sys.error("Method not defined within class"))
                      if(methodExists.argList.length != args.length)
                        sys.error("Method call and method declaration arg list size differs")
                      meth.setSymbol(methodExists)
                      meth
                    case None =>
                      sys.error("Identifier not defined in the current scope")
                  }
                case x: ClassSymbol =>
                  x.lookupVar(value) match {
                    case Some(v) =>
                      obj.asInstanceOf[Identifier].setSymbol(v)
                      val classExists = global.lookupClass(v.getType.toString).getOrElse(sys.error("Class is not defined"))
                      val methodExists = classExists.lookupMethod(meth.value).getOrElse(sys.error("Method not defined within class"))
                      if(methodExists.argList.length != args.length)
                        sys.error("Method call and method declaration arg list size differs")
                      meth.setSymbol(methodExists)
                      meth
                    case None =>
                      sys.error("Identifier not defined in the current scope")
                  }
                case _ => sys.error("Variable not declared in the current scope")
              }
            }
            case New(tpe) =>
              global.lookupClass(tpe.value) match {
                case Some(cls)=>
                  tpe.setSymbol(cls)
                  cls.lookupMethod(meth.value) match {
                    case Some(declaredMeth) =>
                      meth.setSymbol(declaredMeth)
                      meth
                    case None => sys.error("Method has not been declared insied of the class.")
                  }
                case None => sys.error("Class has not been declared.")
              }
            case _ => sys.error("Not a valid obj for the method call")
          }
        case _ => sys.error("Expected a method call")
      }
    }

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
          recurseExpr(obj, sym)
          obj match {
            case x @ MethodCall(obj2, meth2, args2) =>
              var y = recurseMethCall(x,sym)
              global.lookupClass(y.getType.toString) match {
                case Some(classFound) =>
                  classFound.lookupMethod(meth.value) match {
                    case Some(methodFound) =>
                      meth.setSymbol(methodFound)
                    case None =>
                      sys.error("Method not found")
                  }
                case None =>
                  sys.error("Class not found")
              }

            case This() =>
              sym match {
                case x: MethodSymbol =>
                  x.classSymbol.lookupMethod(meth.value) match {
                    case Some(m) =>
                      if(m.argList.length == args.length)
                        meth.setSymbol(m)
                      else
                        sys.error("Wrong number of arguments")
                    case None => sys.error("Method not declared")
                  }
                case x: ClassSymbol =>
                  x.lookupMethod(meth.value) match {
                    case Some(m) =>
                      if(m.argList.length == args.length)
                        meth.setSymbol(m)
                      else
                        sys.error("Wrong number of arguments")
                    case None => sys.error("Method not declared")
                  }
                case _ => sys.error("Variable not declared in the current scope")
              }
              
            case Identifier(value) => {
              sym match {
                case x: MethodSymbol =>
                  x.lookupVar(value) match {
                    case Some(v) =>
                      obj.asInstanceOf[Identifier].setSymbol(v)
                      val classExists = global.lookupClass(v.getType.toString).getOrElse(sys.error("Class is not defined"))
                      val methodExists = classExists.lookupMethod(meth.value).getOrElse(sys.error("Method not defined within class"))
                      if(methodExists.argList.length != args.length)
                        sys.error("Method call and method declaration arg list size differs")
                      meth.setSymbol(methodExists)
                    case None =>
                      sys.error("Identifier not defined in the current scope")
                  }
                case x: ClassSymbol =>
                  x.lookupVar(value) match {
                    case Some(v) =>
                      obj.asInstanceOf[Identifier].setSymbol(v)
                      val classExists = global.lookupClass(v.getType.toString).getOrElse(sys.error("Class is not defined"))
                      val methodExists = classExists.lookupMethod(meth.value).getOrElse(sys.error("Method not defined within class"))
                      if(methodExists.argList.length != args.length)
                        sys.error("Method call and method declaration arg list size differs")
                      meth.setSymbol(methodExists)
                    case None =>
                      sys.error("Identifier not defined in the current scope")
                  }
                case _ => sys.error("Variable not declared in the current scope")
              }
            }
            case New(tpe) =>
              global.lookupClass(tpe.value) match {
                case Some(cls)=>
                  tpe.setSymbol(cls)
                  cls.lookupMethod(meth.value) match {
                    case Some(declaredMeth) =>
                      meth.setSymbol(declaredMeth)
                    case None => sys.error("Method has not been declared insied of the class.")
                  }
                case None => sys.error("Class has not been declared.")
              }
            
            case _ =>
          }
          args.foreach(a =>{
            recurseExpr(a, sym)
          })
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
          sym match {
            case _:MethodSymbol =>
              expr.asInstanceOf[This].setSymbol(sym.asInstanceOf[MethodSymbol].classSymbol)
            case _:ClassSymbol =>
              expr.asInstanceOf[This].setSymbol(sym.asInstanceOf[ClassSymbol])
            case _ =>
              sys.error("This token declared outside of an expected scope")
          }
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
          sym match {
            case x: MethodSymbol =>
              if(x.params.contains(id.value))
                sys.error("Method argument already defined")
            case _ =>
          }
          recurseExpr(id, sym)
          recurseExpr(expr, sym)
        case _ => sys.error("Unrecognized expression")
      }
    }

    def symClasses(): Unit = {
      prog.classes.foreach(cls => {
        val sym = (new ClassSymbol(cls.id.value)).setPos(cls)
        val clsType = new TAnyRef(sym)
        cls.setSymbol(sym)
        cls.id.setSymbol(sym)
        sym.setType(clsType)
        if(global.lookupClass(cls.id.value) == None) {
          global.classes += (cls.id.value -> sym)
        } else {
          sys.error("Class already in global scope")
        }
      })

    }

    def symClassParents(): Unit = {
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

    }

    def symClassVarsAndMethods(): Unit = {
      prog.classes.foreach(cls => {
        cls.vars.foreach(v => {
          val varType = getType(v.tpe)
          val sym = (new VariableSymbol(v.id.value)).setPos(v)
          sym.setType(varType)
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
          val methType = getType(m.retType)
          val sym = (new MethodSymbol(m.id.value, cls.getSymbol)).setPos(m)
          sym.setType(methType)
          m.setSymbol(sym)
          m.id.setSymbol(sym)
          if(m.overrides == true && cls.parent == None)
            sys.error("Method can not override if there is not parent class")

          if(m.overrides == false)
            cls.getSymbol.lookupMethod(m.id.value) match {
              case Some(x) => sys.error("Method " + m.id.value + " already exists in current class " + cls.id.value)
              case None => cls.getSymbol.methods = cls.getSymbol.methods + (m.id.value -> sym)
            }

          m.getSymbol.argList = List[VariableSymbol]()
          m.args.foreach(arg => {
            val argType = getType(arg.tpe)
            val sym = (new VariableSymbol(arg.id.value)).setPos(arg)
            sym.setType(argType)
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


            m.getSymbol.argList.foreach(a => {
              if(a.name == v.id.value)
                sys.error("Shadowing not allowed")
            })

            val varType = getType(v.tpe)
            val sym = (new VariableSymbol(v.id.value)).setPos(v)
            sym.setType(varType)
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

    }


    def symMethodOverrides(): Unit = {

      prog.classes.foreach(cls => {
        cls.methods.foreach(m => {
          if(m.overrides && cls.getSymbol.parent == None)
            sys.error("Method overrides but parent is not defined")
          if(m.overrides) {
            cls.getSymbol.parent.get.lookupMethod(m.id.value) match {
              case x @ Some(overriddenMethod) =>
                if(m.getSymbol.params.size == overriddenMethod.params.size)
                  m.getSymbol.overridden = x
                else
                  sys.error("Method override does not have the same number of params or same type")
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

    }

    def symMain(): Unit = {
      val mainSym = (new ClassSymbol(prog.main.obj.value)).setPos(prog.main)
      global.mainClass = mainSym
      prog.main.setSymbol(mainSym)
      prog.main.obj.setSymbol(mainSym)
      prog.main.vars.foreach(v => {
        val varType = getType(v.tpe)
        val sym = (new VariableSymbol(v.id.value)).setPos(v)
        sym.setType(varType)
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

    }

    symClasses()
    symClassParents()
    symClassVarsAndMethods()
    symMethodOverrides()
    symMain()

    prog
  }

}
