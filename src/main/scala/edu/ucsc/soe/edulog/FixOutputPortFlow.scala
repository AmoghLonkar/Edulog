package edu.ucsc.soe.edulog

import firrtl.ir._
import firrtl.options.{Dependency, PreservesAll}
import firrtl.passes.memlib.Source
import firrtl._
import firrtl.passes.{LowerTypes, Pass}
import firrtl.stage.TransformManager.TransformDependency

/**
 * This Pass fixes an error in which a module output is also used as the input for another expression in the same module
 */
object FixOutputPortFlow extends Pass with PreservesAll[Transform] {
    /**
     * An object of this type is created for every module visited.
     * A namespace is used to create a name (the string) and a wire is created.
     * At the end, the DefWires are placed at the top of the module
     */
    private type WireCollection = collection.mutable.HashMap[String, DefWire]

    override def run(c: Circuit): Circuit = {
        c copy (modules = c.modules map (m => {
            // set up WireCollection and namespace
            val ns = Namespace()
            val wires = new WireCollection()

            // visit each statement, this returns a DefModule.
            val newMod = m mapStmt visitStatement(m.ports.filter(_.direction == Output), wires, ns)
            val newMod2 = newMod.asInstanceOf[Module]

            // also write out the new assignments, put those at the end
            val newOutputConnections = wires.map { case (portName, newWire) => {
                Connect(
                    NoInfo,
                    WRef(portName, newWire.tpe, PortKind, SinkFlow),
                    WRef(newWire.name, newWire.tpe, WireKind, SourceFlow)
                )
            }}

            // we want a module with the wire statements prepended and the final output connections appended
            newMod2 copy (body = Block(wires.values.toSeq ++ Seq(newMod2.body) ++ newOutputConnections.toSeq))
        }) )
    }

    private def visitStatement(modulePorts: Seq[Port], wires: WireCollection, ns: Namespace)(s: Statement): Statement = {
        val ve = visitExpression(modulePorts, wires, ns)(_) // convenience partially-applied func to reduce typing
        val vs = visitStatement(modulePorts, wires, ns)(_)

        s match {
            case c: Connect => c copy (loc = ve(c.loc), expr = ve(c.expr))
            case c: DefNode => c copy (value = ve(c.value))
            case c: PartialConnect => c copy (loc = ve(c.loc), expr = ve(c.expr))
            case c: Block => c mapStmt visitStatement(modulePorts, wires, ns) // blocks done recursively
            case c: Conditionally => c copy (pred = ve(c.pred), conseq = vs(s), alt = vs(s))
            case c: IsInvalid => c copy (expr = ve(c.expr))
            case c: DefInstance => c
            case c: WDefInstance => c
            case c: DefWire => c
            case c: DefRegister => c // although it has clock, reset, init, these don't need to be rewritten
            case EmptyStmt => s // this one is an object for some reason
            case _ => throw new UnsupportedOperationException("not implemented") // TODO. probably not too hard, but not important right now
        }
    }

    private def visitExpression(modulePorts: Seq[Port], wires: WireCollection, ns: Namespace)(expr: Expression): Expression = {
        val v = visitExpression(modulePorts, wires, ns)(_) // convenience partially-applied func to reduce typing

        expr match {
            // recursive cases. check the embedded expression
            // TODO: de-dup somehow?
            case s: SubAccess => s copy (expr = v(s.expr))
            case s: WSubAccess => s copy (expr = v(s.expr))
            case s: SubField => s copy (expr = v(s.expr))
            case s: WSubField => s copy (expr = v(s.expr))
            case s: SubIndex => s copy (expr = v(s.expr))
            case s: WSubIndex => s copy (expr = v(s.expr))
            case s: DoPrim => s copy (args = s.args map v)

            // base cases. references are modified, everything else is just copied
            case r: Reference => r copy (name = maybeCreateWire(r.name, modulePorts, wires, ns))
            case r: WRef => r copy (name = maybeCreateWire(r.name, modulePorts, wires, ns))
            case _ => expr
        }
    }

    /**
     * Given a string corresponding to a net name, check if that is an out port.
     * If so, create a wire and insert it into the [[wires]]. The new name is returned.
     * If not, just return the original name
     * If the wire is an outport and was already seem before, return that name
     * @param name  Name of the wire
     * @param outputPorts The output ports of the current module
     * @param wires The collection of wires for this module
     * @param ns A namespace object to create new names
     * @return The name of the wire, as described
     */
    private def maybeCreateWire(name: String, outputPorts: Seq[Port], wires: WireCollection, ns: Namespace): String = {
        outputPorts.find(p => p.name == name && p.direction == Output) match {
            case Some(p) => {
                // this is an output port
                wires.getOrElseUpdate(name, {
                    // executed when we didn't already have a wire for this one
                    val newName = ns.newName("_fixedoutput")
                    DefWire(NoInfo, newName, p.tpe)
                }).name
            }
            case None => name // this is not an out port, no need to create a wire for it
        }
    }
}
