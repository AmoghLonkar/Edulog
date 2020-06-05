package edu.ucsc.soe.edulog
// based off https://github.com/freechipsproject/firrtl/blob/master/src/main/scala/firrtl/Visitor.scala

import firrtl._
import firrtl.ir.Statement

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
    private val clock = ir.Reference("clock", ir.ClockType)
    
    /**
     * Global implied reset.
     */
    private val reset = ir.Reference("reset", ir.ResetType)
    
    /**
     * Convenience for the default value of a reg.
     * For now it's always a 0.
     */
    private val registerInit = ir.UIntLiteral(0, ir.UnknownWidth)

    /**
     * This is used to create unique identifiers.
     */
    private val moduleNamespace = Namespace()

    /**
     * Cache of the modules in the design. Filled in by visit() so that we know all module names, their inputs and outputs
     */
    private val modules = collection.mutable.HashMap[String, (List[Net], List[Net])]()
    
    /**
     * Main vistor entrypoint.
     */
    def visit(in: List[ModuleDeclaration]): ir.Circuit = {
        // TODO: need to figure out how to get the top level module and make that the name of the Circuit
        // before going into the circuit, first save the modules
        for (m <- in) modules(m.name) = (m.inputs, m.outputs)

        // dive into the AST
        ir.Circuit(ir.NoInfo, in.map(visitModuleDeclaration), "EdulogCircuit")
    }
    
    /**
     * Given a net and a direction, turn it into a FIRRTL port.
     * Since the width has to be specified in Edulog, pass that in.
     * Always returns a UInt to make comparisons later easier
     */
    private def netToPort(dir: ir.Direction, p: Net): ir.Port = {
        ir.Port(visitInfo(p), p.name, dir, ir.UIntType(ir.IntWidth(BigInt(p.high)))) // p.high == p.low here
    }
    
    private def visitModuleDeclaration(in: ModuleDeclaration): ir.DefModule = {
        // map the nets to input and output ports, respectively
        val ports = 
            Seq(
                ir.Port(ir.NoInfo, "clock", ir.Input, ir.ClockType),
                ir.Port(ir.NoInfo, "reset", ir.Input, ir.ResetType)
            ) ++
            in.inputs.map(netToPort(ir.Input, _)) ++ in.outputs.map(netToPort(ir.Output, _))
        
        ir.Module(visitInfo(in), in.name, ports, visitModuleBody(in.body, in.name))
    }
    
    /**
     * Outputs the actual body of the module.
     * An Edulog module consists of at least one statement
     */
    private def visitModuleBody(in: List[Assignment], currentModule: String): ir.Statement = {
        ir.Block(in.map(visitAssignment(_, currentModule)))
    }
    
    /**
     * This is the method that handles the interesting part of Edulog: the assignments
     */
    private def visitAssignment(in: Assignment, currentModule: String): ir.Statement = {
        var mainAsgType = ir.UIntType(ir.UnknownWidth)

        in.right match {
            case RegisterCall(inside) => {
                assert(in.left.length == 1) // can only have one thing to assign to
                var destNet = in.left.head

                ir.Block(Seq(
                    ir.DefRegister(visitInfo(destNet), destNet.name, mainAsgType, clock, reset, registerInit),
                    ir.Connect(visitInfo(inside), ir.Reference(destNet.name, mainAsgType), visitExpr(inside))
                ))
            }
            case ModuleCall(name, inputs) => {
                val instName = moduleNamespace.newName("_modinst")
                val instRef = ir.Reference(instName, ir.UnknownType)

                if (!modules.contains(name)) {
                    throw new Exception(s"module name ${name} not defined")
                }

                val stmts = collection.mutable.ArrayBuffer[ir.Statement]()

                // define the instance
                stmts += ir.DefInstance(visitInfo(in), instName, name)
                stmts += ir.Connect(ir.NoInfo, ir.SubField(instRef, "clock", ir.ClockType), clock)
                stmts += ir.Connect(ir.NoInfo, ir.SubField(instRef, "reset", ir.ResetType), reset)

                // for each input, create a connection to the bundle type
                assert(inputs.length == modules(name)._1.length) // must have all inputs defined
                for ((myExpr, moduleNet) <- inputs zip modules(name)._1) {
                    stmts += ir.Connect(
                        ir.NoInfo,
                        ir.SubField(instRef, moduleNet.name, ir.UnknownType), // input to the module
                        visitExpr(myExpr) // this is the expr we're pulling from for the instance
                    )
                }

                // for each output, create a wire and assign the module output to that wire
                assert(in.left.length == modules(name)._2.length) // must have same amount of outputs as are defined
                for ((myNet, moduleNet) <- in.left zip modules(name)._2) {
                    stmts += ir.DefWire(ir.NoInfo, myNet.name, mainAsgType)
                    stmts += ir.Connect(
                        ir.NoInfo,
                        ir.Reference(myNet.name, mainAsgType), // this is the destination
                        ir.SubField(instRef, moduleNet.name, ir.UnknownType)
                    )
                }

                ir.Block(stmts)
            }
            case e: Expr => {
                assert(in.left.length == 1) // can only have one thing to assign to
                var destNet = in.left.head

                var wireStmt: Statement = ir.DefWire(visitInfo(in), destNet.name, mainAsgType)
                // is the dest an output? if so, no need to create a wire for it
                if (modules(currentModule)._2.map(_.name).contains(destNet.name)) {
                    wireStmt = ir.EmptyStmt
                }

                ir.Block(Seq(
                    wireStmt,
                    ir.Connect(visitInfo(e), ir.Reference(destNet.name, mainAsgType), visitExpr(e))
                ))
            }
            case Mux(cond, inputs) => {
              assert(in.left.length == 1) // can only have one thing to assign to
              var destNet = in.left.head

              ir.Block(Seq(ir.Block.mapExpr(ir.Mux(visitExpr(cond), visitExpr(inputs(0)), visitExpr(inputs(1)), ir.UnknownType))))

            } 
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
                    case BinaryOpType.Addition => PrimOps.Add
                    case BinaryOpType.Subtraction => PrimOps.Sub
                    case BinaryOpType.Multiplication => PrimOps.Mul
                    case BinaryOpType.Division => PrimOps.Div
                    case BinaryOpType.Modulus => PrimOps.Rem
                    case BinaryOpType.LessThan => PrimOps.Lt
                    case BinaryOpType.LessThanOrEquals => PrimOps.Leq
                    case BinaryOpType.GreaterThan => PrimOps.Gt
                    case BinaryOpType.GreaterThanOrEquals => PrimOps.Geq
                    case BinaryOpType.Equals => PrimOps.Eq
                    case BinaryOpType.NotEquals => PrimOps.Neq
                    case BinaryOpType.ShiftLeft => PrimOps.Dshl
                    case BinaryOpType.ShiftRight => PrimOps.Dshr
                    case BinaryOpType.LogicalAnd => PrimOps.And
                    case BinaryOpType.LogicalOr => PrimOps.Or
                }

                // return the op
                ir.DoPrim(firrtlOp, Seq(left, right) map visitExpr, Seq(), ir.UIntType(ir.UnknownWidth))
            }
            case UnaryOp(op, operand) => {
              var firrtlOp = op match {
                case UnaryOpType.Complement => PrimOps.Neg   
                case UnaryOpType.ReduceAnd => PrimOps.Andr   
                case UnaryOpType.ReduceOr => PrimOps.Orr   
                case UnaryOpType.ReduceXor => PrimOps.Xorr   
                }

                ir.DoPrim(firrtlOp, Seq(operand) map visitExpr, Seq(), ir.UIntType(ir.UnknownWidth))
            }
            case Net(name, high, low) => {
                var theRef = ir.Reference(name, ir.UIntType(ir.UnknownWidth))
                if (high == null && low == null) {
                    // take the whole net, whose width is unknown for now
                    theRef
                } else {
                    // extract the bits we want
                    ir.DoPrim(PrimOps.Bits, Seq(theRef), Seq(BigInt(high), BigInt(low)), ir.UIntType(ir.UnknownWidth))
                }
            }

            case NumericLiteral(v) => {
                var tmp = WrappedInt(v).U
                tmp
            }
            
            case SignExtension(operand, width) => {
                // first convert to SInt
                var x = ir.DoPrim(PrimOps.AsSInt, Seq(visitExpr(operand)), Seq(), ir.UIntType(ir.UnknownWidth))

                // then do the pad
                var y = ir.DoPrim(PrimOps.Pad, Seq(x), Seq(width), ir.SIntType(ir.UnknownWidth))

                // now return to UInt
                ir.DoPrim(PrimOps.AsUInt, Seq(y), Seq(), ir.UIntType(ir.UnknownWidth))
            }
            case ZeroExtension(operand, width) => {
                // first convert to UInt
                var x = ir.DoPrim(PrimOps.AsUInt, Seq(visitExpr(operand)), Seq(), ir.UnknownType)

                // then do the pad
                ir.DoPrim(PrimOps.Pad, Seq(x), Seq(width), ir.UIntType(ir.UnknownWidth))
            }
            case Replication(operand, count) => ir.DoPrim(PrimOps.Cat, Seq(visitExpr(operand)), Seq(count), ir.UIntType(ir.UnknownWidth))
            case Concatenation(operands) => ir.DoPrim(PrimOps.Cat, Seq(), Seq(), ir.UIntType(ir.UnknownWidth))
        }
    }
    
    /**
     * Check if the given expression is terminal (is that the right word?)
     * Basically, can it be used as a "ground type" in FIRRTL expressions.
     */
    private def isTerminal(e: Expr): Boolean = e.isInstanceOf[Net] || e.isInstanceOf[NumericLiteral]
}
