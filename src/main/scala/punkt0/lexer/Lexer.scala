package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    var char: Int = source.next()

    var temp_pos = source.pos
    var flag_eof = false
    var token: Token = new Token(BAD)

    new Iterator[Token] {

      def read(): Unit = {
        if(source.hasNext)
          char = source.next()
        else
          char = -1
      }

      def skipSingleComment(): Unit = {
        while(char != '\n' && char != -1)
          read()
      }

      def skipMultiComment(): Unit = {
        var flag = true
        while(char != -1 && flag) {
          read()
          if(char == '*') {
            read()
            if(char == '/') {
              read()
              flag = false
            }
          }
        }
        if(char == -1 && flag == true) 
          Reporter.error("Wrong block comment")
      }

      def skipNewlineOrWhitespace(): Unit = {
        while(char == ' ' || char == '\n') {
          read()
        }
      }

      def nextMatches(matchChar: Char): Boolean = {
        read()
        return matchChar == char
      }

      def stringToken(): Token = {
        read()
        var rtn = ""
        while(char != '\"' && char != -1 && char != '\n') {
          rtn = rtn + char.toChar
          read()
        }
        if(char != '\"') {
          read()
          return new Token(BAD)
        }
        else {
          read()
          return new STRLIT(rtn)
        }
      }

      def intToken(arg: Int): Token = {
        var rtn = 0
        while(char <= '9' && char >= '0') {
          rtn = rtn*10 + (char - 48)
          read()
        }
        return new INTLIT(rtn)
      }

      def keywordOrIdToken(): Token = {
        var rtn = ""
        while((char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || char == '_' || (char >= '0' && char <= '9')) {
          rtn = rtn + char.toChar
          read()
        }
        return rtn match {
          case "object" => new Token(OBJECT)
          case "class" => new Token(CLASS)
          case "def" => new Token(DEF)
          case "override" => new Token(OVERRIDE)
          case "var" => new Token(VAR)
          case "Unit" => new Token(UNIT)
          case "String" => new Token(STRING)
          case "extends" => new Token(EXTENDS)
          case "Int" => new Token(INT)
          case "Boolean" => new Token(BOOLEAN)
          case "while" => new Token(WHILE)
          case "if" => new Token(IF)
          case "else" => new Token(ELSE)
          case "true" => new Token(TRUE)
          case "false" => new Token(FALSE)
          case "this" => new Token(THIS)
          case "null" => new Token(NULL)
          case "new" => new Token(NEW)
          case "println" => new Token(PRINTLN)
          case _ => new ID(rtn)
        }
      }

      def hasNext: Boolean = !flag_eof

      def next() = {

        skipNewlineOrWhitespace()

        temp_pos = source.pos

        char match {
          case -1 => flag_eof = true; token = new Token(EOF).setPos(f, temp_pos)
          case ':' => read(); token = new Token(COLON).setPos(f, temp_pos)
          case ';' => read(); token = new Token(SEMICOLON).setPos(f, temp_pos)
          case '.' => read(); token = new Token(DOT).setPos(f, temp_pos)
          case ',' => read(); token = new Token(COMMA).setPos(f, temp_pos)
          case '!' => read(); token = new Token(BANG).setPos(f, temp_pos)
          case '(' => read(); token = new Token(LPAREN).setPos(f, temp_pos)
          case ')' => read(); token = new Token(RPAREN).setPos(f, temp_pos)
          case '{' => read(); token = new Token(LBRACE).setPos(f, temp_pos)
          case '}' => read(); token = new Token(RBRACE).setPos(f, temp_pos)
          case '<' => read(); token = new Token(LESSTHAN).setPos(f, temp_pos)
          case '+' => read(); token = new Token(PLUS).setPos(f, temp_pos)
          case '-' => read(); token = new Token(MINUS).setPos(f, temp_pos)
          case '*' => read(); token = new Token(TIMES).setPos(f, temp_pos)
          case '=' =>
            read()
            char match {
              case '=' => read(); token = new Token(EQUALS).setPos(f, temp_pos)
              case _ => token = new Token(EQSIGN).setPos(f, temp_pos)
            }
          case '&' =>
            read()
            char match {
              case '&' => read(); token = new Token(AND).setPos(f, temp_pos)
              case _ =>
                Reporter.error("Expected AND token")
                token = new Token(BAD).setPos(f, temp_pos)
            }            
          case '|' =>
            read()
            char match {
              case '|' => read(); token = new Token(OR).setPos(f, temp_pos)
              case _ =>
                Reporter.error("Expected OR token")
                token = new Token(BAD).setPos(f, temp_pos)
            }            
          case '/' =>
            read()
            char match {
              case '/' => skipSingleComment(); token = next()
              case '*' => skipMultiComment(); token = next()
              case _ => token = new Token(DIV).setPos(f, temp_pos)
            }
          case x =>
            if(char <= '9' && char >= '0') {
              token = intToken(char)
              token.setPos(f, temp_pos)
            }
            else if((char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z')) {
              token = keywordOrIdToken()
              token.setPos(f, temp_pos)
            }
            else if(char == '\"') {
              token = stringToken()
              token.setPos(f, temp_pos)
            } else {
              Reporter.error("Bad token found " + x)
              read(); token = new Token(BAD).setPos(f, temp_pos)
            }
        }
        token
      }
    }
  }
}
