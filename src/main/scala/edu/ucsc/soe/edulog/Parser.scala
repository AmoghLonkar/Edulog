package edu.ucsc.soe.edulog

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable.{Map => MutableMap}

sealed trait ASTNode extends Positional
case class ModuleDeclaration(name: String, inputs: List[Net], outputs: List[Net], body: List[Assignment]) extends ASTNode

case class Assignment(left: List[Net], right: AssignmentRHS) extends ASTNode

sealed trait AssignmentRHS extends ASTNode
case class RegisterCall(in: Net) extends AssignmentRHS
case class ModuleCall(name: String, inputs: List[Net]) extends AssignmentRHS
sealed abstract class Expr(width: Integer = null) extends AssignmentRHS

case class Mux(selector: Expr, inputs: List[Expr]) extends Expr


sealed trait BinaryOp extends Expr {
    def op: String
    def left: Expr 
    def right: Expr
}


case class Net(name: String, high: Integer = null, low: Integer = null) extends Expr

class Parser extends StandardTokenParsers {
    lexical.reserved += ("module", "register", "mux")
    lexical.delimiters += (",", ":", "=", "(", ")", "{", "}", "[", "]", "&", "|", "+", "-")
    
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
    
    //def topLevel: Parser[ModuleDeclaration]] = rep(moduleDeclaration)
    //def topLevel: Parser[_] = rep1sep(net, ",") ~ "=" ~ "module" ~ ident ~ "(" ~ repsep(net, ",") ~ ")" ~ "{" ~ rep1(assignment) ~ "}"
    
    /**
     * Parser main function
     */
    def parseAll(in: String): ParseResult[_] = phrase(moduleDeclaration)(new lexical.Scanner(in))
    //def parseAll(in: String): ParseResult[List[Net]] = phrase(topLevel)(new lexical.Scanner(in))
    //def parseAll[T](p: Parser[T], in: String): ParseResult[T] = phrase(p)(new lexical.Scanner(in))
}