package edu.ucsc.soe.edulog

/* based on http://enear.github.io/2016/03/31/parser-combinators/  */

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.RegexParsers

sealed trait Token extends Positional

case class IDENTIFIER(str: String) extends Token
//case class LITERAL(str : String) extends Token
//case class SPACE(spaces : Int) extends Token
case class NUMBER(num: Int) extends Token

case class COMMA() extends Token
case class EQUALS() extends Token
case class COLON() extends Token

case class MODULE() extends Token
case class REGISTER() extends Token

case class OPEN_PAREN() extends Token
case class CLOSE_PAREN() extends Token
//case class OPEN_SQUARE() extends Token
//case class CLOSE_SQUARE() extends Token
case class OPEN_CURLY() extends Token
case class CLOSE_CURLY() extends Token
case class OPEN_COMMENT() extends Token
case class CLOSE_COMMENT() extends Token

case class WIDTH_DECL(n: Int) extends Token
case class INDEXING(high: Int, low: Int) extends Token

case class BITWISE_AND() extends Token
case class BITWISE_OR() extends Token
case class LOGICAL_AND() extends Token
case class LOGICAL_OR() extends Token

trait CompilationError
case class LexerError(msg: String) extends CompilationError

object Lexer extends RegexParsers {
    override def skipWhitespace = true
    
    def identifier: Parser[IDENTIFIER] = {
        "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
    }
    
    def number: Parser[NUMBER] = {
        """"\d+"""".r ^^ { num => NUMBER(num.toInt) }
    }
    
    def comma       = "," ^^ (_ => COMMA())
    def equals      = "=" ^^ (_ => EQUALS())
    def colon       = ":" ^^ (_ => COLON())
    
    def module      = "module" ^^ (_ => MODULE())
    def register    = "register" ^^ (_ => REGISTER())
    
    def openParen   = "(" ^^ (_ => OPEN_PAREN())
    def closeParen  = ")" ^^ (_ => CLOSE_PAREN())
    //def openSquare  = "[" ^^ (_ => OPEN_SQUARE())
    //def closeSquare = "]" ^^ (_ => CLOSE_SQUARE())
    def openCurly   = "{" ^^ (_ => OPEN_CURLY())
    def closeCurly  = "}" ^^ (_ => CLOSE_CURLY())
    
    def widthDecl   : Parser[WIDTH_DECL] = "[" ~ number ~ "]" ^^ {
        case _ ~ n ~ _ => WIDTH_DECL(n.num)
    }
    def indexing    : Parser[INDEXING] = "[" ~ number ~ ":" ~ number ~ "]" ^^ {
        case _ ~ high ~ _ ~ low ~ _ => INDEXING(high.num, low.num)
    }
    
    def bitwiseAnd  = "&" ^^ (_ => BITWISE_AND())
    def bitwiseOr   = "|" ^^ (_ => BITWISE_OR())
    def logicalAnd  = "&&" ^^ (_ => LOGICAL_AND())
    def logicalOr   = "||" ^^ (_ => LOGICAL_OR())
    
    def tokens: Parser[List[Token]] = {
        phrase(rep(/*logicalOr | logicalAnd | bitwiseAnd | bitwiseOr
            | openCurly | closeCurly | */ indexing | widthDecl
            /*| openParen | closeParen | module | register
            | comma | equals | colon | number */ | identifier
        ))
    }
    
    def apply(source: String): Either[LexerError, List[Token]] = {
        parseAll(tokens, source) match {
            case NoSuccess(msg, next)  => Left(LexerError(msg))
            case Success(result, next) => Right(result)
        }
    }
}