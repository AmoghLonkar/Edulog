;buildInfoPackage: chisel3, version: 3.2.6, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit CombLogic : 
  module CombLogic : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip c : UInt<1>, output : UInt<1>}
    
    node _T = not(io.a) @[CombLogic.scala 17:17]
    node _T_1 = and(_T, io.b) @[CombLogic.scala 17:23]
    node _T_2 = not(io.b) @[CombLogic.scala 17:34]
    node _T_3 = and(_T_2, io.a) @[CombLogic.scala 17:40]
    node _T_4 = and(_T_3, io.c) @[CombLogic.scala 17:47]
    node _T_5 = or(_T_1, _T_4) @[CombLogic.scala 17:31]
    io.output <= _T_5 @[CombLogic.scala 17:13]
    
