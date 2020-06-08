import edu.ucsc.soe.edulog._
import firrtl.{CircuitState, Transform, UnknownForm}
import firrtl.passes._

object SimpleTester extends App {
    //val p = new EdulogParser()
    
    val res = EdulogParser.parseAll(
        """
        output[1] = module CombLogic(in_a[1], in_b[1], in_c[1]){


	output = (~in_a & in_b) | (~in_b & in_a & in_c)
        }   


        """
    )
    
    println(res)
    
    val resCircuit = EdulogVisitor.visit(res.get)
    
    println("before transform:")
    println(resCircuit.serialize)
    
    // this part based on https://github.com/freechipsproject/firrtl/blob/master/src/test/scala/firrtlTests/fixed/FixedTypeInferenceSpec.scala
    val passes = Seq[Transform](
        SplitExpressions,
        ToWorkingIR,
        FixOutputPortFlow,
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
