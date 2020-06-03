package edu.ucsc.soe.edulog
// based off https://github.com/freechipsproject/firrtl/blob/master/src/main/scala/firrtl/Visitor.scala

// maybe use: https://github.com/freechipsproject/firrtl/wiki/Common-Pass-Idioms to nest things instead

import firrtl._

object EdulogVisitor {
    /**
     * Creates some annotations for the FIRRTL that shows where the stuff originated.
     * TODO: add the filename into here, need to figure out how to get that
     */
    private def visitInfo(ctx: ASTNode): ir.Info = {
        val infoStr = "line " + ctx.pos.line + " col " + ctx.pos.column
        ir.FileInfo(ir.StringLit(infoStr))
    }
    
    /**
     * This is the global implied clock.
     */
    private var clock = ir.Reference("clock", ir.ClockType)
    
    /**
     * Global implied reset.
     */
    private var reset = ir.Reference("reset", ir.ResetType)
    
    /**
     * Convenience for the default value of a reg.
     * For now it's always a 0.
     */
    private var registerInit = ir.SIntLiteral(0, ir.UnknownWidth)
    
    /**
     * Main vistor entrypoint.
     */
    def visit(in: List[ModuleDeclaration]): ir.Circuit = {
        // TODO: need to figure out how to get the top level module and make that the name of the Circuit
        ir.Circuit(ir.NoInfo, in.map(visitModuleDeclaration), "EdulogCircuit")
    }
    
    /**
     * Given a net and a direction, turn it into a FIRRTL port.
     * Since the width has to be specified in Edulog, pass that in.
     * Always returns a SInt to make comparisons later easier
     */
    private def netToPort(dir: ir.Direction, p: Net): ir.Port = {
        ir.Port(visitInfo(p), p.name, dir, ir.SIntType(ir.IntWidth(BigInt(p.high)))) // p.high == p.low here
    }
    
    private def visitModuleDeclaration(in: ModuleDeclaration): ir.DefModule = {
        // map the nets to input and output ports, respectively
        val ports = 
            Seq(
                ir.Port(ir.NoInfo, "clock", ir.Input, ir.ClockType),
                ir.Port(ir.NoInfo, "reset", ir.Input, ir.ResetType)
            ) ++
            in.inputs.map(netToPort(ir.Input, _)) ++ in.outputs.map(netToPort(ir.Output, _))
        
        ir.Module(visitInfo(in), in.name, ports, visitModuleBody(in.body))
    }
    
    /**
     * Outputs the actual body of the module.
     * An Edulog module consists of at least one statement
     */
    private def visitModuleBody(in: List[Assignment]): ir.Statement = {
        ir.Block(in.map(visitAssignment))
    }
    
    /**
     * This is the method that handles the interesting part of Edulog: the assignments
     */
    private def visitAssignment(in: Assignment): ir.Statement = {
        in.right match {
            case RegisterCall(inside) => {
                assert(in.left.length == 1) // can only have one thing to assign to
                var destNet = in.left.head
                
                var regType = ir.SIntType(ir.UnknownWidth)
                ir.Block(Seq(
                    ir.DefRegister(visitInfo(destNet), destNet.name, regType, clock, reset, registerInit),
                    ir.Connect(visitInfo(in), ir.Reference(destNet.name, regType), visitExpr(inside))
                ))
            }
            //case ModuleCall =>
            /*
            case Expr => {
                assert(in.left.length == 1) // can only have one thing to assign to
                var destNet = in.left.head
                
                ir.Connect(visitInfo(in), )
            }*/
            //case Mux =>
        }
    }
    
    /**
     * Visits the expression.
     * It will return the result of the expression (e.g. a DoPrim)
     * If there are intermediate steps required, those will be prepended to [[extraSteps]], which must be initialized
     */
    private def visitExpr(in: Expr): ir.Expression = {
        in match {
            case BinaryOp(op, left, right) => {
                // convert Edulog op to FIRRTL op
                var firrtlOp = op match {
                    case BinaryOpType.BitwiseAnd => PrimOps.And
                    case BinaryOpType.BitwiseOr => PrimOps.Or
                    case BinaryOpType.BitwiseXor => PrimOps.Xor
                    // TODO: do the rest
                }
                
                // return the op
                ir.DoPrim(firrtlOp, Seq(left, right) map visitExpr, Seq(), ir.SIntType(ir.UnknownWidth))
            }
            //case UnaryOp =>
            case Net(name, high, low) => {
                var theRef = ir.Reference(name, ir.SIntType(ir.UnknownWidth))
                if (high == null && low == null) {
                    // take the whole net, whose width is unknown for now
                    theRef
                } else {
                    // extract the bits we want
                    ir.DoPrim(PrimOps.Bits, Seq(theRef), Seq(BigInt(high), BigInt(low)), ir.SIntType(ir.UnknownWidth))
                }
            }
            case NumericLiteral(v) => ir.SIntLiteral(v, ir.UnknownWidth)
            //case SignExtension =>
            //case ZeroExtension =>
            //case Replication =>
            //case Concatenation =>
        }
    }
    
    /**
     * Check if the given expression is terminal (is that the right word?)
     * Basically, can it be used as a "ground type" in FIRRTL expressions.
     */
    private def isTerminal(e: Expr): Boolean = { e.isInstanceOf[Net] || e.isInstanceOf[NumericLiteral] }
}