import edu.ucsc.soe.edulog._
import firrtl.{CircuitState, Transform, UnknownForm}
import firrtl.passes._

object SimpleTester extends App {
    //val p = new EdulogParser()
    
    val res = EdulogParser.parseAll(
        """
        out1[5], out2[4] = module Bla (in1[5], in2[8]) {
            out1 = register (in1 & in2 & in1 & in2 | in1)

            out2 = out1 sext 5

            uselessNet1, uselessNet2 = Test2 ( 'b1 )

            mux1 = mux(uselessNet1, uselessNet1, uselessNet2)
            //out2 = out1 & out3 == {net1, net6} + 'd98
            
            //out6 = ({a, b, c}) sext 8

        }

        blabla[1], asdf[8] = module Test2 (something[1]) {
            blabla = 'h1

            uselessInternalNet = 'h500

            asdf = something zext 8
        }
    
        """
    )
    
    println(res)
    
    val resCircuit = EdulogVisitor.visit(res.get)
    
    println("before transform:")
    println(resCircuit.serialize)
    
    // this part based on https://github.com/freechipsproject/firrtl/blob/master/src/test/scala/firrtlTests/fixed/FixedTypeInferenceSpec.scala
    val passes = Seq[Transform](
        Splitter,
        ToWorkingIR,
        InferTypes,
        CheckTypes,
        ResolveFlows,
        CheckFlows,
        new InferWidths,
        CheckWidths)
    
    val c = passes.foldLeft(CircuitState(resCircuit, UnknownForm)) {
        (c: CircuitState, t: Transform) => {
            println("=== doing " + t.name + " ===")
            var res1 = t.runTransform(c)
            println("result:")
            println(res1.circuit.serialize)
            res1
        }
    }
    
    println("final result:")
    println(c.circuit.serialize)
    
    
    //val resCircuit3 = firrtl.passes.InferWidths.run(resCircuit2)
    //println("after bitwidth inference:")
    //println(resCircuit3.serialize)
}
