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
sealed trait Expr extends AssignmentRHS
case class RegisterCall(in: Expr) extends AssignmentRHS
case class ModuleCall(name: String, inputs: List[Expr]) extends AssignmentRHS
case class MuxCall(selector: Expr, inputs: List[Expr]) extends AssignmentRHS

object BinaryOpType extends Enumeration {
    type BinaryOpType = Value
    val BitwiseAnd, BitwiseOr, BitwiseXor, 
        Addition, Subtraction, Multiplication, Division, Modulus, 
        LessThan, LessThanOrEquals, GreaterThan, GreaterThanOrEquals,
        Equals, NotEquals, 
        ShiftLeft, ShiftRight,
        LogicalAnd, LogicalOr = Value
}

case class BinaryOp(op: BinaryOpType.Value, left: Expr, right: Expr) extends Expr

object UnaryOpType extends Enumeration {
    type UnaryOpType = Value
    val Complement, ReduceAnd, ReduceOr, ReduceXor = Value
}
case class UnaryOp(op: UnaryOpType.Value, operand: Expr) extends Expr

case class Net(name: String, high: Integer = null, low: Integer = null) extends Expr {
    def isBitExtraction: Boolean = high != null && low != null
    def getExtractSeq: Seq[BigInt] = Seq(BigInt(high), BigInt(low))
}

case class NumericLiteral(value: BigInt) extends Expr // note: width is in Expr
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

    lexical.reserved += ("module", "register", "mux", "sext", "zext", "rep", "clock", "reset")
    lexical.delimiters += (",", ":", "=", "(", ")", "{", "}", "[", "]", "&", "|", "^", "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "<<", ">>", "'", "~", "#")
    
    def moduleDeclaration: Parser[ModuleDeclaration] = positioned { rep1sep(netOne, ",") ~ "=" ~ "module" ~ ident ~ "(" ~ repsep(netOne, ",") ~ ")" ~ "{" ~ rep1(assignment) ~ "}" ^^ {
        case outputs ~ "=" ~ "module" ~ modName ~ "(" ~ inputs ~ ")" ~ "{" ~ assignments ~ "}" => {
            // TODO: check if outputs, inputs are declared correctly
            if (assignments.length == 0) {
                throw new Exception("useless empty module")
            }
            
            ModuleDeclaration(modName, inputs, outputs, assignments)
        }
        case _ => throw new Exception("very bad")
    }}
    
    /**
     * Nets can take the form of ident, ident[n], ident[hi:lo]
     */
    def netHiLo: Parser[Net] = positioned { ident ~ "[" ~ numericLit ~ ":" ~ numericLit ~ "]" ^^ {
        case netName ~ "[" ~ hi ~ ":" ~ lo ~ "]" => Net(netName, hi.toInt, lo.toInt)
    }}
    def netOne: Parser[Net] = positioned { ident ~ "[" ~ numericLit ~ "]" ^^ {
        case netName ~ "[" ~ n ~ "]" => Net(netName, n.toInt, n.toInt)
    }}
    def netNameOnly: Parser[Net] = positioned { ident ^^ {
        case netName => Net(netName, null, null)
    }}
    def net: Parser[Net] = positioned { netHiLo ||| netOne ||| netNameOnly }

    def assignment: Parser[Assignment] = positioned { rep1sep(netNameOnly, ",") ~ "=" ~ assignmentRHS ^^ {
        case destNets ~ "=" ~ rhs => Assignment(destNets, rhs)
    }}
    
    def assignmentRHS: Parser[AssignmentRHS] = positioned { registerCall | moduleCall | expr | mux }
    //def assignmentRHS: Parser[AssignmentRHS] = positioned { muxModuleRegister | expr }

    def registerCall: Parser[RegisterCall] = positioned { "register" ~ "(" ~> expr <~ ")" ^^ {
        case e => RegisterCall(e)
    }}
    
    def moduleCall: Parser[ModuleCall] = positioned { ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
        case modName ~ "(" ~ inputs ~ ")" => ModuleCall(modName, inputs)
    }}

    def mux: Parser[MuxCall] = positioned { "mux" ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
      case "mux" ~ "(" ~ inputs ~ ")" => MuxCall(inputs(0), inputs.tail)
    }}

    /*
    def muxModuleRegister: Parser[AssignmentRHS] = "mux" ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
        case "mux" ~ "(" ~ inputs ~ ")" => Mux(inputs(0), inputs.drop(0))
    } | "register" ~ "(" ~> expr <~ ")" ^^ {
        case e => RegisterCall(e)
    } | "#" ~> ident ~ "(" ~ repsep(net, ",") ~ ")" ^^ {
        case modName ~ "(" ~ inputs ~ ")" => ModuleCall(modName, inputs)
    }
    */

    def bitwiseReduction: Parser[UnaryOp] = positioned { "'" ~ ("&" | "|" | "^") ~ expr ^^ {
        case _ ~ "&" ~ e => UnaryOp(UnaryOpType.ReduceAnd, e)
        case _ ~ "|" ~ e => UnaryOp(UnaryOpType.ReduceOr, e)
        case _ ~ "^" ~ e => UnaryOp(UnaryOpType.ReduceXor, e)
    }}

    def basedHexLit: Parser[NumericLiteral] = positioned { elem("hex lit", _.isInstanceOf[lexical.HexLit]) ^^ {
        case c => NumericLiteral(Integer.parseInt(c.chars, 16))
    }}
    def basedBinLit: Parser[NumericLiteral] = positioned { elem("bin lit", _.isInstanceOf[lexical.BinLit]) ^^ {
        case c => NumericLiteral(Integer.parseInt(c.chars, 2))
    }}
    def basedDecLit: Parser[NumericLiteral] = positioned { elem("dec lit", _.isInstanceOf[lexical.DecLit]) ^^ {
        case c => NumericLiteral(Integer.parseInt(c.chars, 10))
    }}
    def basedNumericLit: Parser[NumericLiteral] = positioned { basedHexLit | basedBinLit | basedDecLit }

    def expr: Parser[Expr] = positioned { exprLogicalOr }

    // here's how this (seems) to work: the * means interleave two of the thing on the left with the parser on the right
    // and the parser on the right is a partial thing? the ^^^ is a shorthand for a converter
    def exprLogicalOr: Parser[Expr]  = positioned { exprLogicalAnd * ("||" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.LogicalOr, a, b) }) }
    def exprLogicalAnd: Parser[Expr] = positioned { exprBitwiseOr * ("&&" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.LogicalAnd, a, b) } ) }
    def exprBitwiseOr: Parser[Expr]  = positioned { exprBitwiseXor * ("|" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.BitwiseOr, a, b) } ) }
    def exprBitwiseXor: Parser[Expr] = positioned {
        exprBitwiseAnd * (
            "^" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.BitwiseXor, a, b) }
            | "~^" ^^^ { (a: Expr, b: Expr) => UnaryOp(UnaryOpType.Complement, BinaryOp(BinaryOpType.BitwiseXor, a, b)) } // don't have XNOR in FIRRTL
        )
    }
    def exprBitwiseAnd: Parser[Expr] = positioned {
        exprEquality * ("&" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.BitwiseAnd, a, b) } )
    }
    def exprEquality: Parser[Expr] = positioned {
        exprInequality * (
            "==" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.Equals, a, b) }
            | "!=" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.NotEquals, a, b) }
        )
    }
    def exprInequality: Parser[Expr] = positioned {
        exprShift * (
            ">" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.GreaterThan, a, b) }
            | ">=" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.GreaterThanOrEquals, a, b) }
            | "<" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.LessThan, a, b) }
            | "<=" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.LessThanOrEquals, a, b) }
        )
    }
    def exprShift: Parser[Expr] = positioned {
        exprAddSub * (
            "<<" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.ShiftLeft, a, b) }
            | ">>" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.ShiftRight, a, b) }
        )
    }
    def exprAddSub: Parser[Expr] = positioned {
        exprMulDiv * (
            "+" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.Addition, a, b) }
            | "-" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.Subtraction, a, b) }
        )
    }
    def exprMulDiv: Parser[Expr] = positioned {
        exprReplication * (
            "*" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.Multiplication, a, b) }
            | "/" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.Division, a, b) }
            | "%" ^^^ { (a: Expr, b: Expr) => BinaryOp(BinaryOpType.Modulus, a, b) }
        )
    }
    def exprReplication: Parser[Expr] = positioned {
        exprConcat ~ opt(("sext" | "zext" | "rep") ~ numericLit) ^^ {
            case e ~ None => e
            case e ~ Some("sext" ~ n) => SignExtension(e, n.toInt)
            case e ~ Some("zext" ~ n) => ZeroExtension(e, n.toInt)
            case e ~ Some("rep" ~ n)  => Replication(e, n.toInt)
        }
    }
    def exprConcat: Parser[Expr] = positioned {
        "{" ~> rep1sep(expr, ",") <~ "}" ^^ { 
            case el => Concatenation(el) // doesn't like the ^^^ form here if you have a list not sure why
        } | exprReductionNegation
    }
    def exprReductionNegation: Parser[Expr] = positioned {
        "~" ~> opt("&" | "|" | "^") ~ expr ^^ {
            case None ~ e => UnaryOp(UnaryOpType.Complement, e)
            case Some("&") ~ e => UnaryOp(UnaryOpType.ReduceAnd, e)
            case Some("|") ~ e => UnaryOp(UnaryOpType.ReduceOr, e)
            case Some("^") ~ e => UnaryOp(UnaryOpType.ReduceXor, e)
        } | exprParens
    }
    def exprParens: Parser[Expr] = positioned {
        "(" ~> expr <~ ")" | // nothing special to do with this expr, just returns it by default
        basedNumericLit |
        net
    }
    
    //def topLevel: Parser[ModuleDeclaration]] = rep(moduleDeclaration)
    //def topLevel: Parser[_] = rep1sep(net, ",") ~ "=" ~ "module" ~ ident ~ "(" ~ repsep(net, ",") ~ ")" ~ "{" ~ rep1(assignment) ~ "}"
    
    /**
     * Parser main function
     */
    def parseAll(in: String): ParseResult[List[ModuleDeclaration]] = phrase(rep1(moduleDeclaration))(new lexical.Scanner(in))
    //def parseAll(in: String): ParseResult[List[Net]] = phrase(topLevel)(new lexical.Scanner(in))
    //def parseAll[T](p: Parser[T], in: String): ParseResult[T] = phrase(p)(new lexical.Scanner(in))
}