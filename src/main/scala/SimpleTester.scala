import edu.ucsc.soe.edulog._

object SimpleTester extends App {
    //val p = new EdulogParser()
    
    val res = EdulogParser.parseAll(
        """
        out1[5], out2[4] = module Bla (in1[5], in2[8]) {
            reg1[5] = register (in1)
            

            out2 = out1 & out3 == {net1, net6} + 'd98
            
        }
    
        """
    )
    
    println(res)
}
