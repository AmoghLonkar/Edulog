# Edulog

Some super simple parsing is now working. To try it, do:

    $ sbt
    sbt:edulog> compile
    sbt:edulog> run

Given this code (see `SimpleTester.scala`):

    out1[5], out2[4] = module Bla (in1[5], in2[8]) {
        reg1[5] = register (in1)
        
        out1, other[8] = OtherModule(reg1, in2)
        
    }
    
You should get:

    [9.9] parsed: ModuleDeclaration(Bla,List(Net(in1,5,5), Net(in2,8,8)),List(Net(out1,5,5), Net(out2,4,4)),List(Assignment(List(Net(reg1,5,5)),RegisterCall(Net(in1,null,null))), Assignment(List(Net(out1,null,null), Net(other,8,8)),ModuleCall(OtherModule,List(Net(reg1,null,null), Net(in2,null,null))))))


Next up is making expressions work as well.