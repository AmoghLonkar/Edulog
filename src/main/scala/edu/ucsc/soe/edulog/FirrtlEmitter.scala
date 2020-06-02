package edu.ucsc.soe.edulog
// based off https://github.com/freechipsproject/firrtl/blob/master/src/main/scala/firrtl/Visitor.scala

import firrtl.ir._

object EdulogVisitor {
    def visit(in: List[ModuleDeclaration]): ir.Circuit = {
        // TODO: make this understand the filename and also add some FIRRTL info
        // might be able to use the Parser Positional stuff? need to look into this
        ir.Circuit(NoInfo, in.map(visitModuleDeclaration), "EdulogCircuit")
    }
    
    /**
     * Given a net and a direction, turn it into a FIRRTL port.
     * Since the width has to be specified in Edulog, pass that in.
     * Always returns a SInt to make comparisons later easier
     */
    private def netToPort(dir: ir.Direction, p: Net): ir.Port = {
        ir.Port(NoInfo, p.name, dir, ir.SIntType(ir.IntWidth(p.high))) // p.high == p.low here
    }
    
    private def visitModuleDeclaration(in: ModuleDeclaration): ir.DefModule = {
        // map the nets to input and output ports, respectively
        // TODO: add the global clock and reset here
        val ports = in.inputs.map(netToPort(ir.Input, _)) ++ in.outputs.map(netToPort(ir.Output, _))
        
        ir.Module(NoInfo, in.name, ports, visitModuleBody(in.body))
    }
    
    /**
     * Outputs the actual body of the module.
     * An Edulog module consists of at least one statement
     */
    private def visitModuleBody(in: List[Assignment]): ir.Statement = {
        ir.Block(in.map(visitAssignment))
    }
    
    private def visitAssignment(in: Assignment): ir.Statement = {
        in.right match {
            case RegisterCall => 
            case ModuleCall =>
            case Expr =>
            case Mux =>
        }
    }

}