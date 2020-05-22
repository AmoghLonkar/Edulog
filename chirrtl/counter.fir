;buildInfoPackage: chisel3, version: 3.2.6, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit Counter : 
  module Counter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input : UInt<3>, flip en : UInt<1>, flip clr : UInt<1>, flip load : UInt<1>, output : UInt<3>}
    
    reg register : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[Counter.scala 18:25]
    when io.clr : @[Counter.scala 19:15]
      register <= UInt<1>("h00") @[Counter.scala 19:25]
      skip @[Counter.scala 19:15]
    else : @[Counter.scala 20:24]
      when io.load : @[Counter.scala 20:24]
        register <= io.input @[Counter.scala 20:35]
        skip @[Counter.scala 20:24]
      else : @[Counter.scala 21:22]
        when io.en : @[Counter.scala 21:22]
          node _T = add(register, UInt<1>("h01")) @[Counter.scala 21:45]
          node _T_1 = tail(_T, 1) @[Counter.scala 21:45]
          register <= _T_1 @[Counter.scala 21:33]
          skip @[Counter.scala 21:22]
        else : @[Counter.scala 22:17]
          register <= register @[Counter.scala 22:28]
          skip @[Counter.scala 22:17]
    io.output <= register @[Counter.scala 24:13]
    
