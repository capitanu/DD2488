package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    val in = source.reader()
    var char: Int = in.read()
    var c: Int = 1
    var l: Int = 1

    var flag_read = true

    new Iterator[Token] {

      def read(): Unit = {
        char = in.read()
        if(char == '\n') {
          c = 0
          l = l + 1
          read()
        }
        else if(char == ' ') {
          c = c + 1
          read()
        }
        else {
          c = c + 1
        }
      }

      def hasNext: Boolean = {
        if(char == -1)
          return false
        return true
      }

      def next() = {

        if(!flag_read) {
          read()
        } else {
          if(char != ' ' && char != '\n')
            flag_read = false
          else {
            flag_read = false
            read()
          }
        }

        var token: Token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))

        char match {
          case -1 => token = new Token(EOF).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case ':' => token = new Token(COLON).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case ';' => token = new Token(SEMICOLON).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '.' => token = new Token(DOT).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case ',' => token = new Token(COMMA).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '=' =>
            char = in.read()
            char match {
              case '\n' =>
                token = new Token(EQSIGN).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
                l = l + 1
                c = -1
              case '=' => token = new Token(EQUALS).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
              case _ =>
                flag_read = true
                token = new Token(EQSIGN).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
            }
            c = c + 1
          case '!' => token = new Token(BANG).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '(' => token = new Token(LPAREN).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case ')' => token = new Token(RPAREN).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '{' => token = new Token(LBRACE).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '}' => token = new Token(RBRACE).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '<' => token = new Token(LESSTHAN).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '+' => token = new Token(PLUS).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '-' => token = new Token(MINUS).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '*' => token = new Token(TIMES).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
          case '&' =>
            char = in.read()
            char match {
              case '\n' =>
                token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
                l = l + 1
                c = -1
              case '&' => token = new Token(AND).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
              case _ => token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
            }
            c = c + 1
          case '|' =>
            char = in.read()
            char match {
              case '\n' =>
                l = l + 1
                c = -1
                token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
              case '|' => token = new Token(OR).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
              case _ => token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
            }
            c = c + 1
          case '/' =>
            char = in.read()
            char match {
              case '\n' =>
                l = l + 1
                c = -1
              case '/' =>
                var currLine = l
                while(currLine == l)
                  read()
                flag_read = true
                token = next()
              case '*' =>
                var flag = true
                while(hasNext && flag) {
                  read()
                  if(char == -1)
                    Reporter.error("Wrong token found")
                  if(char == '*') {
                    char = in.read()
                    if(char == '\n') {
                      l = l + 1
                      c = 0
                    }
                    if(char == '/') {
                      flag = false
                    }
                  }
                }
                c = c + 1
                token = next()
              case _ =>
                flag_read = true
                token = new Token(DIV).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
                c = c + 1
            }
          case _ =>
            if(char <= '9' && char >= '0') {
              var rtn = 0
              var tmp_l = l
              var tmp_c = c
              while(char <= '9' && char >= '0') {
                rtn = rtn * 10 + (char.toInt - 48)
                char = in.read()
                if(char == '\n') {
                  l = l + 1
                  c = 0
                } else {
                  c = c + 1
                }
              }
              flag_read = true
              token = new INTLIT(rtn).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
            }
            else if((char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z')){
              var tmp_l = l
              var tmp_c = c
              var rtn = ""
              while((char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || char == '_' || (char >= '0' && char <= '9')) {
                rtn = rtn + char.toChar
                char = in.read()
                if(char == '\n') {
                  l = l + 1
                  c = 0
                } else {
                  c = c + 1
                }
              }
              flag_read = true
              token = rtn match {
                case "object" => new Token(OBJECT).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "class" => new Token(CLASS).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "def" => new Token(DEF).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "override" => new Token(OVERRIDE).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "var" => new Token(VAR).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "Unit" => new Token(UNIT).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "String" => new Token(STRING).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "extends" => new Token(EXTENDS).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "Int" => new Token(INT).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "Boolean" => new Token(BOOLEAN).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "while" => new Token(WHILE).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "if" => new Token(IF).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "else" => new Token(ELSE).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "true" => new Token(TRUE).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "false" => new Token(FALSE).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "this" => new Token(THIS).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "null" => new Token(NULL).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "new" => new Token(NEW).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case "println" => new Token(PRINTLN).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                case _ => new ID(rtn).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
              }
              
            }
            else if(char == '\"') {
              char = in.read()
              var tmp_l = l
              var tmp_c = c
              var rtn = ""
              c = c + 1
              if(char == -1 || char == '\n')
                token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
              else {
                while(char != '\"') {
                  rtn = rtn + char.toChar
                  char = in.read()
                  if(char == '\n') {
                    l = l + 1
                    c = 0
                  } else {
                    c = c + 1
                  }
                }
                if(tmp_l == l)
                  token = new STRLIT(rtn).setPos(f, (tmp_l << Positioned.COLUMN_BITS) | (tmp_c & Positioned.COLUMN_MASK))
                else
                  token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
              }


            }
            else {
              Reporter.error("Wrong token found")
              token = new Token(BAD).setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
            }
        }

        token

      }
    }
  }
}
