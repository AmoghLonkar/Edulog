import edu.ucsc.soe.edulog._

object SimpleTester extends App {
    //val p = new EdulogParser()
    
    val res = EdulogParser.parseAll(
        """
        out1[5], out2[4] = module Bla (in1[5], in2[8]) {
            out1 = register (in1a & in2a & in1b & in2b | in1c)
            

            //out2 = out1 & out3 == {net1, net6} + 'd98
            
            //out6 = ({a, b, c}) sext 8
            
        }
    
        """
    )
    
    println(res)
    
    val resCircuit = EdulogVisitor.visit(res.get)
    
    println(resCircuit.serialize)
}
