# Edulog

To perform an Edulog to FIRRTL Transform, do:

    $ sbt
    sbt:edulog> compile
    sbt:edulog> run

Given this code (see `SimpleTester.scala`):

     output[1] = module CombLogic(in_a[1], in_b[1], in_c[1]){
	    output = (~in_a & in_b) | (~in_b & in_a & in_c)
     }   

    
You should get:

	[6.9] parsed: List(ModuleDeclaration(CombLogic,List(Net(in_a,1,1), Net(in_b,1,1), Net(in_c,1,1)),List(Net(output,1,1)),List(Assignment(List(Net(output,null,null)),BinaryOp(BitwiseOr,UnaryOp(Complement,BinaryOp(BitwiseAnd,Net(in_a,null,null),Net(in_b,null,null))),UnaryOp(Complement,BinaryOp(BitwiseAnd,BinaryOp(BitwiseAnd,Net(in_b,null,null),Net(in_a,null,null)),Net(in_c,null,null))))))))
	before transform:
	circuit EdulogCircuit :
  	module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    skip
    output <= or(neg(and(in_a, in_b)), neg(and(and(in_b, in_a), in_c))) @[line 3 col 19]

	=== doing firrtl.passes.SplitExpressions$ ===
	result:
	circuit EdulogCircuit :
  	module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    output <= or(_GEN_3, _GEN_4) @[line 3 col 19]

	=== doing firrtl.passes.ToWorkingIR$ ===
	result:
	circuit EdulogCircuit :
  	module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    output <= or(_GEN_3, _GEN_4) @[line 3 col 19]

	=== doing edu.ucsc.soe.edulog.FixOutputPortFlow$ ===
	result:
	circuit EdulogCircuit :
  	module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	=== doing firrtl.passes.InferTypes$ ===
	result:
	circuit EdulogCircuit :
  	module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	=== doing firrtl.passes.CheckTypes$ ===
	result:
	circuit EdulogCircuit :
 	 module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	=== doing firrtl.passes.ResolveFlows$ ===
	result:
	circuit EdulogCircuit :
 	 module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	=== doing firrtl.passes.CheckFlows$ ===
	result:
	circuit EdulogCircuit :
 	 module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	=== doing firrtl.passes.InferWidths ===
	result:
	circuit EdulogCircuit :
 	 module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	=== doing firrtl.passes.CheckWidths$ ===
	result:
	circuit EdulogCircuit :
	  module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput

	final result:
	circuit EdulogCircuit :
  	module CombLogic : @[line 2 col 8]
    input clock : Clock
    input reset : Reset
    input in_a : UInt<1> @[line 2 col 37]
    input in_b : UInt<1> @[line 2 col 46]
    input in_c : UInt<1> @[line 2 col 55]
    output output : UInt<1> @[line 2 col 8]

    wire _fixedoutput : UInt<1>
    skip
    node _GEN_0 = and(in_a, in_b) @[line 3 col 19]
    node _GEN_1 = and(in_b, in_a) @[line 3 col 19]
    node _GEN_2 = and(_GEN_1, in_c) @[line 3 col 19]
    node _GEN_3 = neg(_GEN_0) @[line 3 col 19]
    node _GEN_4 = neg(_GEN_2) @[line 3 col 19]
    _fixedoutput <= or(_GEN_3, _GEN_4) @[line 3 col 19]
    output <= _fixedoutput
