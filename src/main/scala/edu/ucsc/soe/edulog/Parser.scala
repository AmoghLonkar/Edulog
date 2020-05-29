package edu.ucsc.soe.edulog

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input.CharArrayReader.EofCh

sealed trait ASTNode extends Positional
case class ModuleDeclaration(name: String, inputs: List[Net], outputs: List[Net], body: List[Assignment]) extends ASTNode

case class Assignment(left: List[Net], right: AssignmentRHS) extends ASTNode

sealed trait AssignmentRHS extends ASTNode
case class RegisterCall(in: Net) extends AssignmentRHS
case class ModuleCall(name: String, inputs: List[Net]) extends AssignmentRHS
sealed abstract class Expr(width: Integer = null) extends AssignmentRHS

case class Mux(selector: Expr, inputs: List[Expr]) extends Expr

object BinaryOpType extends Enumeration {
    val BitwiseAnd = Value("&")
    val BitwiseOr = Value("|")
    val Addition = Value("+")
    val Subtraction = Value("-")
    val Multiplication = Value("*")
    val Division = Value("/")
    val Modulus = Value("%")
    val LessThan = Value("<")
    val LessThanEqualTo = Value("<=")
    val GreaterThan = Value(">")
    val GreaterThanEqualTo = Value(">=")
    val Equals = Value("==")
    val NotEquals = Value("!=")
    val ShiftLeft = Value("<<")
    val ShiftRight = Value(">>")
    val Xor = Value("^")   
}
case class BinaryOp(op: BinaryOpType.Value, left: Expr, right: Expr) extends Expr

object UnaryOpType extends Enumeration {
    val Complement = Value("~")
    val ReduceAnd, ReduceOr, ReduceXor = Value // these don't have a manual value since they're parsed differently    
}
case class UnaryOp(op: UnaryOpType.Value, operand: Expr) extends Expr

case class Net(name: String, high: Integer = null, low: Integer = null) extends Expr

case class NumericLiteral(value: Int) extends Expr // note: width is in Expr
object NumericLiteralBase extends Enumeration {
    type NumericLiteralBase = Value
    val Decimal, Hexadecimal, Binary = Value
}

/**
 * Parser class for numeric types. Needs to be separate since it's not possible with StandardTokenParsers
 */
// TODO: get rid of all of this and subclass StdLexical to modify lexer to accept based numbers
//   can probably use https://github.com/stephentu/scala-sql-parser/blob/master/src/main/scala/parser.scala for inspiration

/**
 * Main parser class
 */
object EdulogParser extends StandardTokenParsers {
    class EdulogLexical extends StdLexical {
        sealed trait EdulogNumericLiteral extends Token
        case class HexLit(chars: String) extends EdulogNumericLiteral
        case class BinLit(chars: String) extends EdulogNumericLiteral
        case class DecLit(chars: String) extends EdulogNumericLiteral

        override def token: Parser[Token] = (
            identChar ~ rep(identChar | digit) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
            | '\'' ~ 'h' ~ rep1(hexDigit) ^^ { case _ ~ _ ~ digits => HexLit(digits mkString "") }
            | '\'' ~ 'b' ~ rep1(binDigit) ^^ { case _ ~ _ ~ digits => BinLit(digits mkString "") }
            | '\'' ~ 'd' ~ rep1(decDigit) ^^ { case _ ~ _ ~ digits => DecLit(digits mkString "") }
            | EofCh     ^^^ EOF
            | delim
            | failure("illegal character")
        )

        def hexDigit = elem("hex digit", "01234567890abcdefABCDEF".contains(_))
        def binDigit = elem("bin digit", "01".contains(_))
        def decDigit = elem("dec digit", "01234567890".contains(_))
    }
    override val lexical = new EdulogLexical

    lexical.reserved += ("module", "register", "mux", "reduce")
    lexical.delimiters += (",", ":", "=", "(", ")", "{", "}", "[", "]", "&", "|", "^", "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "<<", ">>", "'")
    
    def moduleDeclaration: Parser[ModuleDeclaration] = rep1sep(net, ",") ~ "=" ~ "module" ~ ident ~ "(" ~ repsep(net, ",") ~ ")" ~ "{" ~ rep1(assignment) ~ "}" ^^ {
        case outputs ~ "=" ~ "module" ~ modName ~ "(" ~ inputs ~ ")" ~ "{" ~ assignments ~ "}" => {
            // TODO: check if outputs, inputs are declared correctly
            ModuleDeclaration(modName, inputs, outputs, assignments)
        }
        case _ => throw new Exception("very bad")
    }
    
    /**
     * Nets can take the form of ident, ident[n], ident[hi:lo]
     */
    def net: Parser[Net] = {
        (ident ~ "[" ~ numericLit ~ ":" ~ numericLit ~ "]") ^^ {
            case netName ~ "[" ~ hi ~ ":" ~ lo ~ "]" => Net(netName, hi.toInt, lo.toInt)
        } |
        (ident ~ "[" ~ numericLit ~ "]") ^^ {
            case netName ~ "[" ~ n ~ "]" => Net(netName, n.toInt, n.toInt)
        } |
        (ident) ^^ {
            case netName => Net(netName, null, null)
        }
    }
    
    def assignment: Parser[Assignment] = rep1sep(net, ",") ~ "=" ~ assignmentRHS ^^ {
        case destNets ~ "=" ~ rhs => Assignment(destNets, rhs)
    }
    
    def assignmentRHS: Parser[AssignmentRHS] = registerCall | moduleCall /* | Expr */ 
    
    def registerCall: Parser[RegisterCall] = "register" ~ "(" ~> net <~ ")" ^^ {
        case theNet => RegisterCall(theNet)
    }
    
    def moduleCall: Parser[ModuleCall] = ident ~ "(" ~ repsep(net, ",") ~ ")" ^^ {
        case modName ~ "(" ~ inputs ~ ")" => ModuleCall(modName, inputs)
    }
    
    def bitwiseReduction: Parser[UnaryOp] = "reduce" ~ ("&" | "|" | "^") ~ expr ^^ {
        case _ ~ "&" ~ e => UnaryOp(UnaryOpType.ReduceAnd, e)
        case _ ~ "|" ~ e => UnaryOp(UnaryOpType.ReduceOr, e)
        case _ ~ "^" ~ e => UnaryOp(UnaryOpType.ReduceXor, e)
    }
   
    /*
    def mux: Parser[Mux] = "mux" ~ "(" ~ expr ~ ")" ~ "{" ~ rep1(muxCase) ~ "}" ^^ {
        case _ ~ _ ~ sel ~ _ ~ _ ~ cases ~ _ => {
            // TODO: check that all cases are handled and that the cases array has the correct width
            Mux(sel, cases.sortWith(_._1 < _._1).map(_ => _._2))  // muxCase returns a tuple, sort by first element
        }
    }
    
    def muxCase: Parser[Tuple2[Integer, Expr]] = basedNumericLit ~ ":" ~ expr ^^ {
        case n ~ ":" ~ e => (n, e)
    }*/

    def basedHexLit: Parser[NumericLiteral] = elem("hex lit", _.isInstanceOf[lexical.HexLit]) ^^ NumericLiteral(Integer.parseInt(_.chars, 16))
    def basedBinLit: Parser[NumericLiteral] = elem("bin lit", _.isInstanceOf[lexical.BinLit]) ^^ NumericLiteral(Integer.parseInt(_.chars, 2))
    def basedDecLit: Parser[NumericLiteral] = elem("dec lit", _.isInstanceOf[lexical.DecLit]) ^^ NumericLiteral(Integer.parseInt(_.chars, 10))
    def basedNumericLit: Parser[NumericLiteral] = basedHexLit | basedBinLit | basedDecLit

    def expr: Parser[Expr] = net | basedNumericLit    
    
    //def topLevel: Parser[ModuleDeclaration]] = rep(moduleDeclaration)
    //def topLevel: Parser[_] = rep1sep(net, ",") ~ "=" ~ "module" ~ ident ~ "(" ~ repsep(net, ",") ~ ")" ~ "{" ~ rep1(assignment) ~ "}"
    
    /**
     * Parser main function
     */
    def parseAll(in: String): ParseResult[_] = phrase(moduleDeclaration)(new lexical.Scanner(in))
    //def parseAll(in: String): ParseResult[List[Net]] = phrase(topLevel)(new lexical.Scanner(in))
    //def parseAll[T](p: Parser[T], in: String): ParseResult[T] = phrase(p)(new lexical.Scanner(in))
}

/*

out1[5], out2[4] = module Bla (in1[5], in2[8]) {
    reg1[5] = register (in1) 
    
    out1, other[8] = OtherModule(reg1, in2)
    
    out2 = other[3:0]
    
    out2 = mux (other[3:0]) {
        b0010: a & b
    }
}


out_a[5], out_b[4] = module OtherModule (in_a[5], in_b[8]) {
    ...
}


(out1, other[8]) = OtherModule(reg1, in2)
*/
