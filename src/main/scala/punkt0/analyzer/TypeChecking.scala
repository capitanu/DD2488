package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {

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
      case _ => sys.error("No such type defined")
    }

    def typeCheckVar(v: VarDecl): Type = v.tpe match {
      case IntType() => typeCheckExpr(v.expr, TInt)
      case BooleanType() => typeCheckExpr(v.expr, TBoolean)
      case StringType() => typeCheckExpr(v.expr, TString)
      case UnitType() => typeCheckExpr(v.expr, TUnit)
      case Identifier(value) =>
        typeCheckExpr(v.expr)
        if(!v.expr.getType.isSubTypeOf(v.tpe.getType))
          sys.error("Declared type and actual type do not match")
        TUnit
      case _ => sys.error("No valid type for the variable")
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
            sys.error("Types do not match")
          }
        case MethodCall(obj, meth, args) =>
          obj.setType(typeCheckExpr(obj))
          var i: Int = 0
          args.foreach(a => {
            typeCheckExpr(a)
            if(!a.getType.isSubTypeOf(meth.getSymbol.asInstanceOf[MethodSymbol].argList(i).getType))
              sys.error("Argument has wrong type")
            i = i + 1
          })
          expr.setType(meth.getSymbol.getType)
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
        case Not(expr) =>
          typeCheckExpr(expr, TBoolean)
          expr.setType(TBoolean)
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
                case (TAnyRef(_), TUnit) =>
                  expr.setType(thn.getType)
                  thn.getType
                case (TNull, TAnyRef(_)) =>
                  expr.setType(ee.getType)
                  ee.getType
                case (TUnit, TAnyRef(_)) =>
                  expr.setType(ee.getType)
                  ee.getType
                case (TAnyRef(x), TAnyRef(y)) =>
                  //Fix inheritance minimum match
                  sys.error("Needs implementation")
                case (x, y) =>
                  if (x == y) {
                    expr.setType(x)
                    x
                  } else {
                    sys.error("Then and else branches don't have matching type")
                  }

              }

            case None =>
              typeCheckExpr(thn)
              thn.setType(thn.getType)
              expr.setType(TUnit)
              TUnit
          }
        case While(cond, body) =>
          typeCheckExpr(cond, TBoolean)
          typeCheckExpr(body)
          expr.setType(body.getType)
          body.getType
        case Println(ex) =>
          typeCheckExpr(ex, TString, TInt, TBoolean)
          expr.setType(TUnit)
          TUnit
        case Assign(id, ex) =>
          typeCheckExpr(id)
          typeCheckExpr(ex)
          if(!ex.getType.isSubTypeOf(id.getType))
            sys.error("Invalid assign type")
          expr.setType(TUnit)
          TUnit
      }


      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }


    }

    def typeCheckClass(): Unit = {
      prog.classes.foreach( cls => {

        cls.vars.foreach( v => {
          typeCheckVar(v)
          typeCheckExpr(v.expr, v.getSymbol.getType)

        })

        cls.methods.foreach( m => {
          m.vars.foreach(v => {
            typeCheckVar(v)
            typeCheckExpr(v.expr, v.getSymbol.getType)
          })

          m.exprs.foreach(e => {
            typeCheckExpr(e)
          })

          typeCheckExpr(m.retExpr)
          setType(m.retType)

          println(m.retType.getType)
          println(m.retExpr.getType)
          if(m.retExpr.getType.isSubTypeOf(m.retType.getType) == false)
            sys.error("Return type declared and return type found do not match")

          if(m.overrides) {
            var index = 0
            m.args.foreach(a => {
              setType(a.tpe)
              if(!a.tpe.getType.isSubTypeOf(m.getSymbol.overridden.get.argList(index).getType))
                sys.error("Arg list pattern not matching")
              index += 1
            })

            if(!m.getSymbol.overridden.get.getType.isSubTypeOf(m.getSymbol.getType))
              sys.error("Method return doesn't match the overridden method return type")
          }
        })

      })
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
        sys.error("Main should extend from App")
    }

    def typeCheckProg(): Unit = {
      typeCheckClass()
      typeCheckMain()
    }

    typeCheckProg()

    prog
  }

}
