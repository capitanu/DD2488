package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    val global = new GlobalScope

    def setType(t: TypeTree): Unit = t match {
      case IntType() => t.setType(TInt)
      case BooleanType() => t.setType(TBoolean)
      case StringType() => t.setType(TString)
      case UnitType() => t.setType(TUnit)
      case Identifier(value) =>
        prog.classes.foreach(cls => {
          if(cls.id.value == value)
            t.setType(TAnyRef(cls.getSymbol))
        })
      case _ =>
        TUntyped
    }

    def getType(tpe: TypeTree): Type = tpe match {
      case IntType() => TInt
      case BooleanType() => TBoolean
      case StringType() => TString
      case UnitType() => TUnit
      case x @ UntypedType() => TUntyped(x.id)
      case Identifier(v) =>
        var cls = global.lookupClass(v).getOrElse(sys.error("Type does not exist"))
        tpe.asInstanceOf[Identifier].setSymbol(cls)
        cls.getType
      case _ => sys.error("Type does not exist")
    }

    def typeToTPE(tp: Type): TypeTree = tp match {
      case TInt => IntType()
      case TBoolean => BooleanType()
      case TUnit => UnitType()
      case TString => StringType()
      case TUntyped(x) =>
        var rtn = UntypedType()
        rtn.id = x
        rtn
      case TAnyRef(x) => Identifier(x.name)
      case _ => sys.error("Not a valid type")
    }

    def inferType(exp: ExprTree, sym: Symbol, tpe: TypeTree): TypeTree = exp match {
      case IntLit(_) | Minus(_, _) | Times(_, _) | Div(_, _) => IntType()
      case StringLit(_) => StringType()
      case True() | False() | And(_, _) | Or(_, _) | Equals(_, _) | LessThan(_, _) | Not(_) => BooleanType()
      case New(tpe) =>
        global.lookupClass(tpe.value) match {
          case Some(x) => Identifier(x.name)
          case None => tpe
        }
      case x @ Identifier(_) =>
        sym match {
          case _:ClassSymbol =>
            sym.asInstanceOf[ClassSymbol].lookupVar(x.value) match {
              case Some(vr) =>
                vr.getType match {
                  case TInt => IntType()
                  case TBoolean => BooleanType()
                  case TString => StringType()
                  case TUnit => UnitType()
                  case TNull => UnitType()
                  case TAnyRef(clssym) => Identifier(clssym.name)
                  case _ => tpe
                }
              case None =>
                global.lookupClass(x.value) match {
                  case Some(y) => Identifier(x.value)
                  case None => tpe
                }
            }
          case _: MethodSymbol =>
            sym.asInstanceOf[MethodSymbol].lookupVar(x.value) match {
              case Some(vr) =>
                vr.getType match {
                  case TInt => IntType()
                  case TBoolean => BooleanType()
                  case TString => StringType()
                  case TUnit => UnitType()
                  case TNull => UnitType()
                  case TAnyRef(clssym) => Identifier(clssym.name)
                  case _ => tpe
                }
              case None =>
                global.lookupClass(x.value) match {
                  case Some(y) => Identifier(x.value)
                  case None => tpe
                }
            }
          case _ => tpe
        }
      case Plus(lhs, rhs) =>
        (inferType(lhs, sym, tpe), inferType(rhs, sym, tpe)) match {
          case (IntType(), IntType()) => IntType()
          case (IntType(), StringType()) => StringType()
          case (StringType(), IntType()) => StringType()
          case (StringType(), StringType()) => StringType()
          case _ => tpe
        }
      case This() =>
        Identifier(sym.asInstanceOf[ClassSymbol].name)
      case Null() => UnitType()
      case Block(exprs) =>
        inferType(exprs.last, sym, tpe)
      case If(expr, thn, els) =>
        els match {
          case Some(x) => inferType(x, sym, tpe)
          case None => UnitType()
        }
      case Println(_) => UnitType()
      case While(cond, body) => inferType(body, sym, tpe)
      case Assign(_, _) => UnitType()
      case mc @ MethodCall(obj, meth, args) =>
        //   recurseMethCall(mc, sym)
        tpe
      case _ => tpe
    }


    def recurseMethCall(expr: ExprTree, sym: Symbol): TypeTree = {
      expr match {
        case MethodCall(obj, meth, args) =>
          // traverseMethod(expr, sym)
          recurseExpr(obj, sym)
          // args.foreach(a => {
          //   a.setType(getType(inferType(a, sym, UntypedType())))
          //   println(a.getType)
          // })
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
                      var i = 0
                      if(m.argList.length == args.length) {
                        m.argList.foreach(a => {
                          a.setType(args(i).getType)
                          i = i + 1
                        })
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
            cls.getSymbol.members += (v.getSymbol.name -> v.getSymbol)
            recurseExpr(v.expr, cls.getSymbol)
          } else {
            sys.error("Variable already declared in class scope")
          }
        })

        cls.methods.foreach(m => {
          if(m.overrides == false) {

            val methType = getType(m.retType)
            val sym = (new MethodSymbol(m.id.value, cls.getSymbol)).setPos(m)
            sym.setType(methType)
            m.setSymbol(sym)
            m.id.setSymbol(sym)
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


          }
        })


        cls.methods.foreach(m => {
          if(m.overrides == false) {

            m.vars.foreach(v => {
              m.getSymbol.argList.foreach(a => {
                if(a.name == v.id.value)
                  sys.error("Shadowing not allowed")
              })

              if(v.tpe == UntypedType()) {
                var inferedType = inferType(v.expr, m.getSymbol, v.tpe)
                if(inferedType != v.tpe) {
                  v.tpe = inferedType
                }
              }
              val varType = getType(v.tpe)
              val sym = (new VariableSymbol(v.id.value)).setPos(v)
              sym.setType(varType)
              v.setSymbol(sym)
              v.id.setSymbol(sym)

              if(m.getSymbol.members.contains(v.id.value)) {
                sys.error("Var already declared in method")
              } else {
                recurseExpr(v.expr, m.getSymbol)
                m.getSymbol.members = m.getSymbol.members + (v.id.value -> sym)
              }
            })
          }
        })
      })

    }


    def symMethodOverrides(): Unit = {

      prog.classes.foreach(cls => {
        cls.methods.foreach(m => {
          if(m.overrides == false && cls.getSymbol.parent != None) {
            cls.getSymbol.parent.get.lookupMethod(m.id.value) match {
              case Some(overridenMethod) =>
                sys.error("Method overrides, but not declared to override")
              case None =>
            }
          }
          if(m.overrides && cls.getSymbol.parent == None)
            sys.error("Method overrides but parent is not defined")

          if(m.overrides) {
            cls.getSymbol.parent.get.lookupMethod(m.id.value) match {
              case x @ Some(overriddenMethod) =>

                val methType = getType(m.retType)

                // if(methType != overriddenMethod.getType)
                //   sys.error("Method override does not have the same type as the overriden method")

                val sym = (new MethodSymbol(m.id.value, cls.getSymbol)).setPos(m)

                sym.setType(methType)
                m.setSymbol(sym)
                m.id.setSymbol(sym)

                cls.getSymbol.methods = cls.getSymbol.methods + (m.id.value -> m.getSymbol)
                m.getSymbol.overridden = x
                
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

                if(m.getSymbol.params.size != overriddenMethod.params.size)
                  sys.error("Method override does not have the same number of params")



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

                  var par = cls.getSymbol.parent
                  while(par != None) {
                    par.get.lookupVar(v.id.value) match {
                      case Some(vprime) => sys.error("Variable already declared in parent class")
                      case None => par = par.get.parent
                    }
                  }

                  recurseExpr(v.expr, cls.getSymbol)
                  m.getSymbol.members = m.getSymbol.members + (v.id.value -> sym)
                })



              case None => sys.error("No overriden method, however declared as overrides.")
            }
          }
        })

        cls.methods.foreach(m => {
          m.exprs.foreach(e => {
            recurseExpr(e, m.getSymbol)
          })
          recurseExpr(m.retExpr, m.getSymbol)
          m.getSymbol.setType(m.retExpr.getType)
          m.retType = typeToTPE(m.retExpr.getType)
        })
      })

    }

    def symMain(): Unit = {
      val mainSym = (new ClassSymbol(prog.main.obj.value)).setPos(prog.main)
      val appSym = (new ClassSymbol("App"))
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
        recurseExpr(v.expr, global.mainClass)
        global.mainClass.members = global.mainClass.members + (v.id.value -> sym)
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
