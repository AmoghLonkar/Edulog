import edu.ucsc.soe.edulog.Parser

object SimpleTester extends App {
    val p = new Parser()
    
    val res = p.parseAll(
        """
        out1[5], out2[4] = module Bla (in1[5], in2[8]) {
            reg1[5] = register (in1)
            
            out1, other[8] = OtherModule(reg1, in2)
            
        }
    
        """
    )
    
    println(res)
}