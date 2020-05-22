module CombLogic(
  input   clock,
  input   reset,
  input   io_a,
  input   io_b,
  input   io_c,
  output  io_output
);
  wire  _T; // @[CombLogic.scala 17:17]
  wire  _T_1; // @[CombLogic.scala 17:23]
  wire  _T_2; // @[CombLogic.scala 17:34]
  wire  _T_3; // @[CombLogic.scala 17:40]
  wire  _T_4; // @[CombLogic.scala 17:47]
  assign _T = ~io_a; // @[CombLogic.scala 17:17]
  assign _T_1 = _T & io_b; // @[CombLogic.scala 17:23]
  assign _T_2 = ~io_b; // @[CombLogic.scala 17:34]
  assign _T_3 = _T_2 & io_a; // @[CombLogic.scala 17:40]
  assign _T_4 = _T_3 & io_c; // @[CombLogic.scala 17:47]
  assign io_output = _T_1 | _T_4; // @[CombLogic.scala 17:13]
endmodule
