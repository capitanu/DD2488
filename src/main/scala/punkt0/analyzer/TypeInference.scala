package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeInference extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {

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


    def helperNumeric(expr: ExprTree, lhs: ExprTree, rhs: ExprTree): Type = {
      typeCheckExpr(lhs, TInt)
      typeCheckExpr(rhs, TInt)
      expr.setType(TInt)
      TInt
    }

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

    def typeCheckVar(v: VarDecl): Type = v.tpe match {
      case IntType() => typeCheckExpr(v.expr, TInt)
      case BooleanType() => typeCheckExpr(v.expr, TBoolean)
      case StringType() => typeCheckExpr(v.expr, TString)
      case UnitType() => typeCheckExpr(v.expr, TUnit)
      case Identifier(value) =>
        typeCheckExpr(v.expr)
        if(!v.expr.getType.isSubTypeOf(v.tpe.getType)) {
          Reporter.error("Declared type and actual type do not match")
        }
        TUnit
      case x @ UntypedType() => TUntyped(x.id)

    }

    def typeCheckExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) =>
          typeCheckExpr(lhs, TBoolean)
          typeCheckExpr(rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case Or(lhs, rhs) =>
          typeCheckExpr(lhs, TBoolean)
          typeCheckExpr(rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case Plus(lhs, rhs) =>
          typeCheckExpr(lhs, TInt, TString)
          typeCheckExpr(rhs, TInt, TString)

          if(lhs.getType == rhs.getType) {
            expr.setType(rhs.getType)
            rhs.getType
          } else {
            expr.setType(TString)
            TString
          }
        case Minus(lhs, rhs) =>
          helperNumeric(expr, lhs, rhs)
        case Times(lhs, rhs) =>
          helperNumeric(expr, lhs, rhs)
        case Div(lhs, rhs) =>
          helperNumeric(expr, lhs, rhs)
        case LessThan(lhs, rhs) =>
          typeCheckExpr(lhs, TInt)
          typeCheckExpr(rhs, TInt)
          expr.setType(TBoolean)
          TBoolean
        case Equals(lhs, rhs) =>
          typeCheckExpr(lhs, TInt, TString, TBoolean, anyRef)
          typeCheckExpr(rhs, TInt, TString, TBoolean, anyRef)

          if(lhs.getType == rhs.getType || (lhs.getType.isSubTypeOf(anyRef) && rhs.getType.isSubTypeOf(anyRef))) {
            expr.setType(TBoolean)
            TBoolean
          } else {
            Reporter.error("Types do not match: " + lhs.getType + " != " + rhs.getType)
            expr.setType(TError)
            TError
          }
        case MethodCall(obj, meth, args) =>
          obj.setType(typeCheckExpr(obj))
          var i: Int = 0
          args.foreach(a => {
            typeCheckExpr(a)
            if(!a.getType.isSubTypeOf(meth.getSymbol.asInstanceOf[MethodSymbol].argList(i).getType)){
              meth.getSymbol.asInstanceOf[MethodSymbol].argList(i).getType match {
                case TUntyped(_) =>
                  meth.getSymbol.asInstanceOf[MethodSymbol].argList(i).setType(a.getType)
                case _ =>
                  expr.setType(TError)
                  return TError
              }
            }
            i = i + 1
          })

          try {
            expr.setType(meth.getSymbol.getType)
          } catch {
            case _: Throwable =>
              var methSym: MethodSymbol = null
              prog.classes.foreach(c => {
                if(c.id.value == obj.getType.toString) {
                  meth.setSymbol(c.getSymbol.asInstanceOf[ClassSymbol].lookupMethod(meth.value).getOrElse(sys.error("No matching function found")))
                }
              })
              expr.setType(meth.getSymbol.getType)
          }
          meth.getSymbol.getType
        case IntLit(_) => expr.setType(TInt); TInt
        case StringLit(_) => expr.setType(TString); TString

        case True() | False()  =>
          expr.setType(TBoolean)
          TBoolean
        case Identifier(value) =>
          expr.setType(expr.asInstanceOf[Identifier].getType)
          expr.asInstanceOf[Identifier].getType
        case This() =>
          expr.setType(expr.asInstanceOf[This].getSymbol.getType)
          expr.asInstanceOf[This].getSymbol.getType
        case Null() =>
          expr.setType(TNull)
          TNull
        case New(tpe) =>
          typeCheckExpr(tpe, anyRef)
          expr.setType(tpe.asInstanceOf[Identifier].getType)
          tpe.asInstanceOf[Identifier].getType
        case ex @ Not(ex1) =>
          typeCheckExpr(ex1, TBoolean)
          ex.setType(TBoolean)
          TBoolean
        case Block(exprs) =>
          if(exprs.isEmpty) {
            expr.setType(TUnit)
            TUnit
          } else {
            var tempType: Type = TUnit
            exprs.foreach(e => {
              typeCheckExpr(e)
              tempType = e.getType
            })
            expr.setType(tempType)
            tempType
          }
        case If(ex, thn, els) =>
          typeCheckExpr(ex, TBoolean)
          els match {
            case Some(ee) =>
              typeCheckExpr(thn)
              typeCheckExpr(ee)
                (thn.getType, ee.getType) match {
                case (TAnyRef(_), TNull) =>
                  expr.setType(thn.getType)
                  thn.getType
                case (TNull, TAnyRef(_)) =>
                  expr.setType(ee.getType)
                  ee.getType
                case (TAnyRef(x), TAnyRef(y)) =>
                  if(x == y) {
                    expr.setType(thn.getType)
                    thn.getType
                  }
                  else {
                    var thenClassSym: ClassSymbol = null
                    var elseClassSym: ClassSymbol = null
                    prog.classes.foreach(c => {
                      if(c.id.value == ee.getType.toString())
                        elseClassSym = c.getSymbol
                      if(c.id.value == thn.getType.toString())
                        thenClassSym = c.getSymbol
                    })
                    var tmpElseClassSym: ClassSymbol = elseClassSym
                    while(thenClassSym != null) {
                      while(tmpElseClassSym != null) {
                        if(tmpElseClassSym == thenClassSym) {
                          expr.setType(tmpElseClassSym.getType)
                          return tmpElseClassSym.getType
                        }
                        tmpElseClassSym = tmpElseClassSym.getParentSym
                      }
                      tmpElseClassSym = elseClassSym
                      thenClassSym = thenClassSym.getParentSym
                    }
                    expr.setType(anyRef)
                    anyRef
                  }
                case (x, y) =>
                  if (x == y) {
                    expr.setType(x)
                    x
                  } else {
                    Reporter.error("Then and else branches don't have matching type: " + x + " and " + y)
                    expr.setType(TError)
                    TError
                  }
              }

            case None =>
              typeCheckExpr(thn, TUnit)
              thn.setType(thn.getType)
              expr.setType(TUnit)
              TUnit
          }
        case While(cond, body) =>
          typeCheckExpr(cond, TBoolean)
          typeCheckExpr(body, TUnit)
          expr.setType(body.getType)
          body.getType
        case Println(ex) =>
          typeCheckExpr(ex, TString, TInt, TBoolean)
          expr.setType(TUnit)
          TUnit
        case Assign(id, ex) =>
          typeCheckExpr(id)
          typeCheckExpr(ex)
          if(!ex.getType.isSubTypeOf(id.getType)) {
            Reporter.error("Invalid assign type: id " + id.getType + " and expr " + ex.getType)
            expr.setType(TError)
            TError
          } else {
            expr.setType(TUnit)
            TUnit
          }
      }

      if (expected.isEmpty) {
        tpe
      } else {
        tpe
      }


    }

    def typeCheckClass(): Unit = {
      var flag = true
      var count = 0
      while(flag && count < 10) {
        flag = false
        count = count + 1
        prog.classes.foreach( cls => {
          cls.vars.foreach( v => {
            if(typeToTPE(typeCheckVar(v)) == UntypedType() && typeToTPE(v.getSymbol.getType) != UntypedType()) {
              if(typeToTPE(v.getSymbol.getType) == UntypedType() && typeToTPE(typeCheckExpr(v.expr)) != UntypedType()) {
                v.getSymbol.setType(typeCheckExpr(v.expr))
                v.tpe = typeToTPE(typeCheckExpr(v.expr))
                flag = true
              }
            }

            typeCheckVar(v)
            typeCheckExpr(v.expr, v.getSymbol.getType)
          })

          cls.methods.foreach( m => {
            m.vars.foreach(v => {
              if(typeToTPE(v.getSymbol.getType) == UntypedType() && typeToTPE(typeCheckExpr(v.expr)) != UntypedType()) {
                v.getSymbol.setType(typeCheckExpr(v.expr))
                v.tpe = typeToTPE(typeCheckExpr(v.expr))
                flag = true
              }
              typeCheckVar(v)
              typeCheckExpr(v.expr, v.getSymbol.getType)
            })

            m.exprs.foreach(e => {
              typeCheckExpr(e)
            })

            typeCheckExpr(m.retExpr)
            if(m.retType == UntypedType() && typeToTPE(typeCheckExpr(m.retExpr)) != UntypedType()) {
              m.retType = typeToTPE(typeCheckExpr(m.retExpr))
              m.retType.setType(typeCheckExpr(m.retExpr))
              m.getSymbol.setType(typeCheckExpr(m.retExpr))
              flag = true
            }

            if(m.overrides) {
              var index = 0
              m.args.foreach(a => {
                setType(a.tpe)
                index += 1
              })
            }
          })

        })
      }
    }

    def typeCheckMain(): Unit = {

      prog.main.vars.foreach(v => {
        typeCheckVar(v)
        typeCheckExpr(v.expr, v.getSymbol.getType)
      })

      prog.main.exprs.foreach(e => {
        typeCheckExpr(e)
      })

      if(prog.main.parent.value != "App")
        Reporter.error("Main should extend from App")
    }

    def typeCheckProg(): Unit = {
      typeCheckClass()
      typeCheckMain()
    }

    typeCheckProg()

    prog
  }

}
