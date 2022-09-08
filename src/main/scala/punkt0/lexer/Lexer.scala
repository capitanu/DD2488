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

    new Iterator[Token] {

      def read(): Unit = {
        char = in.read()
        while(char == '\n') {
          c = -1
          l = l + 1
          read()
        }
        while(char == ' ') {
          read()
        }
        c = c + 1
      }

      def hasNext: Boolean = {
        if(char == -1)
          return false
        return true
      }

      def next() = {       

        var token: Token = new Token(BAD)

        char match {
          case -1 => token = new Token(EOF)
          case ':' => token = new Token(COLON)
          case ';' => token = new Token(SEMICOLON)
          case '.' => token = new Token(DOT)
          case ',' => token = new Token(COMMA)
          case '=' =>
            read()
            char match {
              case '=' => token = new Token(EQUALS)
              case _ => token = new Token(EQSIGN)
            }
          case '!' => token = new Token(BANG)
          case '(' => token = new Token(LPAREN)
          case ')' => token = new Token(RPAREN)
          case '{' => token = new Token(LBRACE)
          case '}' => token = new Token(RBRACE)
          case '<' => token = new Token(LESSTHAN)
          case '+' => token = new Token(PLUS)
          case '-' => token = new Token(MINUS)
          case '&' =>
            read()
            char match {
              case '&' => token = new Token(AND)
              case _ => token = new Token(BAD)
            }
          case '|' =>
            read()
            char match {
              case '|' => token = new Token(OR)
              case _ => token = new Token(BAD)
            }
          case '/' =>
            read()
            char match {
              case '/' =>
                var currLine = l
                while(currLine == l)
                  read()
                c = c - 1
              case '*' =>
                var flag = true
                while(hasNext && flag) {
                  read()
                  if(char == '*') {
                    read()
                    if(char == '/') {
                      flag = false
                    }
                  }
                }
              case _ =>
                token = new Token(DIV)
            }


          case _ =>
            Reporter.error("Wrong token found")
            token = new Token(BAD)
        }

        token.setPos(f, (l << Positioned.COLUMN_BITS) | (c & Positioned.COLUMN_MASK))
        read()
        token

      }
    }
  }
}
