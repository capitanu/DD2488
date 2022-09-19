package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next()
        while (currentToken.kind == BAD) {
          currentToken = tokens.next()
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def extendsParser: Option[Identifier] = {
      if(currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        val parentClass = currentToken.asInstanceOf[ID].value
        eat(IDKIND)
        return Option(Identifier(parentClass))
      } else {
        return None
      }
    }

    def typeParser: TypeTree = {
      val tokenKind = currentToken.kind
      var rtn: TypeTree = null
      tokenKind match {
        case BOOLEAN =>
          rtn = BooleanType().setPos(currentToken)
          eat(BOOLEAN)
        case INT => rtn = IntType().setPos(currentToken)
          eat(INT)
        case STRING => rtn = StringType().setPos(currentToken)
          eat(STRING)
        case UNIT => rtn = UnitType().setPos(currentToken)
          eat(UNIT)
        case IDKIND =>
          rtn = Identifier(currentToken.asInstanceOf[ID].value)
          eat(IDKIND)
        case _ =>
          expected(IDKIND, BOOLEAN, INT, STRING, UNIT)
      }
      return rtn
    }

    def exprParser: ExprTree = {
      var expr: ExprTree = null
      expr = priorityOr
      return expr
    }

    def priorityOr: ExprTree = {
      var exprAnd = priorityAnd
      while(currentToken.kind == OR) {
        eat(OR)
        exprAnd = Or(exprAnd, priorityAnd).setPos(exprAnd)
      }
      return exprAnd
    }

    def priorityAnd: ExprTree = {
      var exprLEQorEQ = priorityLEQorEQ
      while(currentToken.kind == AND) {
        eat(AND)
        exprLEQorEQ = And(exprLEQorEQ, priorityLEQorEQ).setPos(exprLEQorEQ)
      }
      return exprLEQorEQ
    }

    def priorityLEQorEQ: ExprTree = {
      var exprPLUSorMINUS = priorityPLUSorMINUS
      while(currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        if(currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          exprPLUSorMINUS = LessThan(exprPLUSorMINUS, priorityPLUSorMINUS).setPos(exprPLUSorMINUS)
        }
        if(currentToken.kind == EQUALS) {
          eat(EQUALS)
          exprPLUSorMINUS = Equals(exprPLUSorMINUS, priorityPLUSorMINUS).setPos(exprPLUSorMINUS)
        }
      }

      return exprPLUSorMINUS
    }

    def priorityPLUSorMINUS: ExprTree = {
      var exprTIMESorDIV = priorityTIMESorDIV
      while(currentToken.kind == PLUS || currentToken.kind == MINUS) {
        if(currentToken.kind == PLUS) {
          eat(PLUS)
          exprTIMESorDIV = Plus(exprTIMESorDIV, priorityTIMESorDIV).setPos(exprTIMESorDIV)
        }
        if(currentToken.kind == MINUS) {
          eat(MINUS)
          exprTIMESorDIV = Minus(exprTIMESorDIV, priorityTIMESorDIV).setPos(exprTIMESorDIV)
        }
      }

      return exprTIMESorDIV
    }

    def priorityTIMESorDIV: ExprTree = {
      var exprToken = priorityToken
      while(currentToken.kind == TIMES || currentToken.kind == DIV) {
        if(currentToken.kind == TIMES) {
          eat(TIMES)
          exprToken = Times(exprToken, priorityToken)
        }
        if(currentToken.kind == DIV) {
          eat(DIV)
          exprToken = Div(exprToken, priorityToken)
        }
      }
      return exprToken
    }

    def priorityToken: ExprTree = {
      var tokenPos = currentToken
      dotToken(
        tokenPos.kind match {
          case THIS =>
            var rtn = This().setPos(tokenPos)
            eat(THIS)
            rtn
          case TRUE =>
            var rtn = True().setPos(tokenPos)
            eat(TRUE)
            rtn
          case FALSE =>
            var rtn = False().setPos(tokenPos)
            eat(FALSE)
            rtn
          case IDKIND =>
            var rtn = Identifier(currentToken.asInstanceOf[ID].value).setPos(tokenPos)
            eat(IDKIND)
            if(currentToken.kind == EQSIGN) {
              eat(EQSIGN)
              Assign(rtn, exprParser).setPos(tokenPos)
            } else {
              rtn
            }
          case INTLITKIND =>
            var rtn = IntLit(currentToken.asInstanceOf[INTLIT].value).setPos(tokenPos)
            eat(INTLITKIND)
            rtn
          case STRLITKIND =>
            var rtn = StringLit(currentToken.asInstanceOf[STRLIT].value).setPos(tokenPos)
            eat(STRLITKIND)
            rtn
          case NULL =>
            var rtn = Null().setPos(tokenPos)
            eat(NULL)
            rtn
          case NEW =>
            eat(NEW)
            if(currentToken.kind != IDKIND)
              expected(IDKIND)
            var rtn = New(Identifier(currentToken.asInstanceOf[ID].value).setPos(currentToken)).setPos(tokenPos)
            eat(IDKIND)
            eat(LPAREN)
            eat(RPAREN)
            rtn
          case BANG =>
            eat(BANG)
            var rtn = Not(priorityToken).setPos(tokenPos)
            rtn
          case PRINTLN =>
            eat(PRINTLN)
            eat(LPAREN)
            var rtn = Println(exprParser).setPos(tokenPos)
            eat(RPAREN)
            rtn
          case LPAREN =>
            eat(LPAREN)
            var rtn = exprParser.setPos(tokenPos)
            eat(RPAREN)
            rtn
          case LBRACE =>
            eat(LBRACE)
            var listRtn: List[ExprTree] = List[ExprTree]()
            while(currentToken.kind != RBRACE) {
              listRtn = listRtn :+ exprParser
              if(currentToken.kind == SEMICOLON)
                eat(SEMICOLON)
              else {
                if(currentToken.kind != RBRACE)
                  expected(RBRACE)
              }
            }
            eat(RBRACE)
            var rtn = Block(listRtn).setPos(tokenPos)
            rtn
          case IF =>
            eat(IF)
            eat(LPAREN)
            var cond = exprParser
            eat(RPAREN)
            var trueBranch = exprParser
            var falseBranch: Option[ExprTree] = None
            if(currentToken.kind == ELSE) {
              eat(ELSE)
              falseBranch = Some(exprParser)
            }
            var rtn = If(cond, trueBranch, falseBranch).setPos(tokenPos)
            rtn
          case WHILE =>
            eat(WHILE)
            eat(LPAREN)
            var cond = exprParser
            eat(RPAREN)
            var body = exprParser
            var rtn = While(cond, body).setPos(tokenPos)
            rtn
          case _ =>
            expected(BANG, NEW, NULL, IDKIND, FALSE, TRUE, THIS, IF, WHILE, PRINTLN, INTLITKIND, STRLITKIND)
        })
    }

    def dotToken(expr: ExprTree): ExprTree = {
      var tokenPos = currentToken
      if(currentToken.kind == DOT) {
        eat(DOT)
        if(currentToken.kind != IDKIND)
          expected(IDKIND)

        var methodId = Identifier(currentToken.asInstanceOf[ID].value).setPos(currentToken)
        eat(IDKIND)
        eat(LPAREN)
        var methodArgs: List[ExprTree] = List[ExprTree]()
        while(currentToken.kind != RPAREN) {
          methodArgs = methodArgs :+ exprParser
          if(currentToken.kind != RPAREN){
            eat(COMMA)
            if(currentToken.kind == RPAREN)
              expected(IDKIND)
          }
        }
        eat(RPAREN)
        
        return dotToken(MethodCall(expr, methodId, methodArgs).setPos(tokenPos))

      } else {
        return expr
      }
    }


    def varsParser: List[VarDecl] = {
      var varList: List[VarDecl] = List[VarDecl]()
      while(currentToken.kind == VAR) {
        varList = varList :+ singleVarParser
      }
      varList
    }

    def singleVarParser: VarDecl = {
      eat(VAR)
      var varID: Identifier = Identifier(currentToken.asInstanceOf[ID].value)
      eat(IDKIND)
      eat(COLON)
      var typeTree: TypeTree = typeParser
      eat(EQSIGN)
      var exprDecl: ExprTree = exprParser
      eat(SEMICOLON)

      return VarDecl(typeTree, varID, exprDecl).setPos(varID)
    }

    def methodsParser: List[MethodDecl] = {
      var methodsList: List[MethodDecl] = List[MethodDecl]()
      while(currentToken.kind == DEF || currentToken.kind == OVERRIDE) {
        var overrides: Boolean = false
        if(currentToken.kind == OVERRIDE) {
          overrides = true
          eat(OVERRIDE)
        }
        eat(DEF)
        var methodID: Identifier = Identifier(currentToken.asInstanceOf[ID].value)
        eat(IDKIND)
        eat(LPAREN)
        var argsList: List[Formal] = List[Formal]()
        while(currentToken.kind != RPAREN) {
          var argID: Identifier = Identifier(currentToken.asInstanceOf[ID].value)
          eat(IDKIND)
          eat(COLON)
          var argType: TypeTree = typeParser
          argsList = argsList :+ Formal(argType, argID)
          if(currentToken.kind != RPAREN) {
            eat(COMMA)
            if(currentToken.kind == RPAREN)
              expected(IDKIND)
          }
        }
        eat(RPAREN)
        eat(COLON)
        var retType: TypeTree = typeParser
        eat(EQSIGN)
        eat(LBRACE)
        var varsList: List[VarDecl] = List[VarDecl]()
        var exprsList: List[ExprTree] = List[ExprTree]()
        while(currentToken.kind == VAR) {
          varsList = varsList :+ singleVarParser
        }
        exprsList = exprsList :+ exprParser
        while(currentToken.kind == SEMICOLON) {
          eat(SEMICOLON)
          exprsList = exprsList :+ exprParser
        }
        eat(RBRACE)
        var retExpr: ExprTree = exprsList.last
        exprsList = exprsList.dropRight(1)

        methodsList = methodsList :+ MethodDecl(overrides, retType, methodID, argsList, varsList, exprsList, retExpr)
      }

      return methodsList
    }

    def classParser: List[ClassDecl] = {

      var clsList: List[ClassDecl] = List[ClassDecl]()

      while(currentToken.kind == CLASS) {
        val classPos = currentToken
        eat(CLASS)

        val className: Identifier = Identifier(currentToken.asInstanceOf[ID].value)
        eat(IDKIND)

        val extendsName: Option[Identifier] = extendsParser

        eat(LBRACE)
        var vars: List[VarDecl] = varsParser
        var methods: List[MethodDecl] = methodsParser
        eat(RBRACE)

        clsList = clsList :+ ClassDecl(className, extendsName, vars, methods).setPos(classPos)
      }

      return clsList
    }

    def mainParser: MainDecl = {
      val mainPos = currentToken
      eat(OBJECT)
      val mainID: Identifier = Identifier(currentToken.asInstanceOf[ID].value)
      eat(IDKIND)
      eat(EXTENDS)
      val parentID: Identifier = Identifier(currentToken.asInstanceOf[ID].value)
      eat(IDKIND)
      eat(LBRACE)
      var varsList: List[VarDecl] = List[VarDecl]()
      var exprsList: List[ExprTree] = List[ExprTree]()
      while(currentToken.kind == VAR) {
          varsList = varsList :+ singleVarParser
      }
      exprsList = exprsList :+ exprParser
      while(currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprsList = exprsList :+ exprParser
      }
      eat(RBRACE)

      return MainDecl(mainID, parentID, varsList, exprsList)
    }

    def parseGoal: Program = {
      val programPos = currentToken

      var cls: List[ClassDecl] = classParser
      var main: MainDecl = mainParser

      eat(EOF)

      return Program(main, cls).setPos(programPos)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
