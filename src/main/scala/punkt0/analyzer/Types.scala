package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
    def getTypeStr: String = {
      try getType.toString
      catch {
        case t: Throwable => "??"
      }
    }
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "Boolean"
  }
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }
  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "Unit"
  }
  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TNull => true
      case _ => false
    }
    override def toString = "Null"
  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = 
      (tpe == anyRef || tpe == classSymbol.getType || (classSymbol.parent != None && classSymbol.parent.get.getType.isSubTypeOf(tpe)))
    override def toString = classSymbol.name
  }

  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}