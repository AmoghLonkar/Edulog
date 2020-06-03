/**
 * This file adapted from https://github.com/freechipsproject/firrtl/wiki/Common-Pass-Idioms/1374fc99ecd1d8844c613aa7b45882fee0b70a50
 *
 * It's distributed under the FIRRTL licence terms.
 */
package edu.ucsc.soe.edulog

import firrtl.ir._
import firrtl.passes.Pass
import firrtl._

object Splitter extends Pass {
    override def name = "Splitter!"
    
    /** Run splitM on every module **/
    def run(c: Circuit): Circuit = c.copy(modules = c.modules map(splitM(_)))

    /** Run splitS on the body of every module **/
    def splitM(m: DefModule): DefModule = m mapStmt splitS(Namespace(m))

    /** Run splitE on all children Expressions.
    * If stmts contain extra statements, return a Block containing them and 
    *    the new statement; otherwise, return the new statement. */
    def splitS(namespace: Namespace)(s: Statement): Statement = {
        val block = collection.mutable.ArrayBuffer[Statement]()
        s match {
            case s: HasInfo => {
                val newStmt = s mapExpr splitE(block, namespace, s.info)
                block.length match {
                    case 0 => newStmt
                    case _ => Block(block.toSeq :+ newStmt)
                }
            }
            case s => s mapStmt splitS(namespace)
        }
    }

    /** Run splitE on all children expressions.
    * If e is a DoPrim, add a new DefNode to block and return reference to
    * the DefNode; otherwise return e.*/
    def splitE(block: collection.mutable.ArrayBuffer[Statement], namespace: Namespace, 
             info: Info)(e: Expression): Expression = e mapExpr splitE(block, namespace, info) match {
        case e: DoPrim => {
            val newName = namespace.newTemp
            block += DefNode(info, newName, e)
            Reference(newName, e.tpe)
        }
        case _ => e
    }
}