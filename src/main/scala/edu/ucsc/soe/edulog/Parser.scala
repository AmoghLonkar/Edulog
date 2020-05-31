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
    val LogicalAnd = Value("and")
    val LogicalOr = Value("or")
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

case class SignExtension(operand: Expr, totalWidth: Int) extends Expr
case class ZeroExtension(operand: Expr, totalWidth: Int) extends Expr
case class Replication(operand: Expr, count: Int) extends Expr

case class Concatenation(operands: List[Expr]) extends Expr

/**
 * Parser class for numeric types. Needs to be separate since it's not possible with StandardTokenParsers
 */
// TODO: get rid of all of this and subclass StdLexical to modify lexer to accept based numbers
//   can probably use https://github.com/stephentu/scala-sql-parser/blob/master/src/main/scala/parser.scala for inspiration

/**
 * Main parser class
 */
object EdulogParser extends StandardTokenParsers {
    // pull in the enum values
    import BinaryOpType._

    class EdulogLexical extends StdLexical {
        sealed trait EdulogNumericLiteral extends Token
        case class HexLit(chars: String) extends EdulogNumericLiteral
        case class BinLit(chars: String) extends EdulogNumericLiteral
        case class DecLit(chars: String) extends EdulogNumericLiteral

        override def token: Parser[Token] = (
            identChar ~ rep(identChar | digit) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
            | rep1(digit) ^^ { case i => NumericLit(i mkString "") }
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

    lexical.reserved += ("module", "register", "mux")
    lexical.delimiters += (",", ":", "=", "(", ")", "{", "}", "[", "]", "&", "|", "^", "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "<<", ">>", "'", "and", "or")
    
    def moduleDeclaration: Parser[ModuleDeclaration] = rep1sep(netOne, ",") ~ "=" ~ "module" ~ ident ~ "(" ~ repsep(netOne, ",") ~ ")" ~ "{" ~ rep1(assignment) ~ "}" ^^ {
        case outputs ~ "=" ~ "module" ~ modName ~ "(" ~ inputs ~ ")" ~ "{" ~ assignments ~ "}" => {
            // TODO: check if outputs, inputs are declared correctly
            ModuleDeclaration(modName, inputs, outputs, assignments)
        }
        case _ => throw new Exception("very bad")
    }
    
    /**
     * Nets can take the form of ident, ident[n], ident[hi:lo]
     */
    def netHiLo: Parser[Net] = ident ~ "[" ~ numericLit ~ ":" ~ numericLit ~ "]" ^^ {
        case netName ~ "[" ~ hi ~ ":" ~ lo ~ "]" => Net(netName, hi.toInt, lo.toInt)
    }
    def netOne: Parser[Net] = ident ~ "[" ~ numericLit ~ "]" ^^ {
        case netName ~ "[" ~ n ~ "]" => Net(netName, n.toInt, n.toInt)
    }
    def netNameOnly: Parser[Net] = ident ^^ {
        case netName => Net(netName, null, null)
    }
    def net: Parser[Net] = netHiLo ||| netOne ||| netNameOnly

    def assignment: Parser[Assignment] = rep1sep(net, ",") ~ "=" ~ assignmentRHS ^^ {
        case destNets ~ "=" ~ rhs => Assignment(destNets, rhs)
    }
    
    def assignmentRHS: Parser[AssignmentRHS] = registerCall | moduleCall | expr
    
    def registerCall: Parser[RegisterCall] = "register" ~ "(" ~> net <~ ")" ^^ {
        case theNet => RegisterCall(theNet)
    }
    
    def moduleCall: Parser[ModuleCall] = ident ~ "(" ~ repsep(net, ",") ~ ")" ^^ {
        case modName ~ "(" ~ inputs ~ ")" => ModuleCall(modName, inputs)
    }
    
    def bitwiseReduction: Parser[UnaryOp] = "'" ~ ("&" | "|" | "^") ~ expr ^^ {
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

    def basedHexLit: Parser[NumericLiteral] = elem("hex lit", _.isInstanceOf[lexical.HexLit]) ^^ {
        case c => NumericLiteral(Integer.parseInt(c.chars, 16))
    }
    def basedBinLit: Parser[NumericLiteral] = elem("bin lit", _.isInstanceOf[lexical.BinLit]) ^^ {
        case c => NumericLiteral(Integer.parseInt(c.chars, 2))
    }
    def basedDecLit: Parser[NumericLiteral] = elem("dec lit", _.isInstanceOf[lexical.DecLit]) ^^ {
        case c => NumericLiteral(Integer.parseInt(c.chars, 10))
    }
    def basedNumericLit: Parser[NumericLiteral] = basedHexLit | basedBinLit | basedDecLit

    def expr: Parser[Expr] = exprLogicalOr

    // here's how this (seems) to work: the * means interleave two of the thing on the left with the parser on the right
    // and the parser on the right is a partial thing? the ^^^ is a shorthand for a converter
    def exprLogicalOr: Parser[Expr] = exprLogicalAnd * ("||" ^^^ { (a, b) => BinaryOp(BinaryOpType.LogicalOr, a, b) })
    def exprLogicalAnd: Parser[Expr] = exprBitwiseOr * ("&&" ^^^ { (a, b) => BinaryOp(BinaryOpType.LogicalAnd, a, b) } )
    def exprBitwiseOr: Parser[Expr] = exprBitwiseXor * ("|" ^^^ { (a, b) => BinaryOp(BinaryOpType.BitwiseOr, a, b) } )
    def exprBitwiseXor: Parser[Expr] =
        exprEquality * (
            "^" ^^^ { (a, b) => BinaryOp(BinaryOpType.BitwiseXor, a, b) }
            | "~^" ^^^ { (a, b) => UnaryOp(UnaryOpType.Complement, BinaryOp(BinaryOpType.BitwiseXor, a, b)) } ) // don't have XNOR in FIRRTL
       // )
    def exprEquality: Parser[Expr] =
        exprInequality * (
            "==" ^^^ { (a, b) => BinaryOp(BinaryOpType.Equals, a, b) }
            | "!=" ^^^ { (a, b) => BinaryOp(BinaryOptype.NotEquals, a, b) }
        )
    def exprInequality: Parser[Expr] =
        exprShift * (
            ">" ^^^ { (a, b) => BinaryOp(BinaryOpType.GreaterThan, a, b) }
            | ">=" ^^^ { (a, b) => BinaryOp(BinaryOpType.GreaterThanOrEquals, a, b) }
            | "<" ^^^ { (a, b) => BinaryOp(BinaryOpType.LessThan, a, b) }
            | "<=" ^^^ { (a, b) => BinaryOp(BinaryOpType.LessThanOrEquals, a, b) }
        )
    def exprShift: Parser[Expr]] =
        exprAddSub * (
            "<<" ^^^ { (a, b) => BinaryOp(BinaryOpType.ShiftLeft, a, b) }
            | ">>" ^^ { (a, b) => BinaryOp(BinaryOpType.ShiftRight, a, b) }
        )
    def exprAddSub: Parser[Expr] =
        exprMulDiv * (
            "+" ^^^ { (a, b) => BinaryOp(BinaryOpType.Add, a, b) }
            | "-" ^^^ { (a, b) => BinaryOp(BinaryOpType.Sub, a, b) }
        )
    def exprMulDiv: Parser[Expr] =
        exprReplication * (
            "*" ^^^ { (a, b) => BinaryOp(BinaryOpType.Multiply, a, b) }
            | "/" ^^^ { (a, b) => BinaryOp(BinaryOpType.Divide, a, b) }
            | "%" ^^^ { (a, b) => BinaryOp(BinaryOpType.Modulus, a, b) }
        )
    def exprReplication: Parser[Expr] = 
        exprConcat ~ opt(("sext" | "zext" | "rep") ~ numericLit) ^^ {
            case e ~ None => e
            case e ~ Some("sext" ~ n) => SignExtension(e, n)
            case e ~ Some("zext" ~ n) => ZeroExtension(e, n)
            case e ~ Some("rep" ~ n) => Replication(e, n)
        }
    def exprConcat: Parser[Expr] =
        "{" ~> rep1sep(expr, ",") <~ "}" ^^ (_ => Concatenation(_))
        | exprReductionNegation
    def exprReductionNegation: Parser[Expr] =
        "~" ~> expr ^^^ (_ => UnaryOp(UnaryOpType.Complement, _))     |
        "~" ~ "&" ~> expr ^^^ (_ => UnaryOp(UnaryOpType.ReduceAnd, _))  | 
        "~" ~ "|" ~> expr ^^^ (_ => UnaryOp(UnaryOpType.ReduceOr, _))   |
        "~" ~ "^" ~> expr ^^^ (_ => UnaryOp(UnaryOpType.ReduceXor, _))
   
    
        
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
