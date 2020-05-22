module Counter(
  input        clock,
  input        reset,
  input  [2:0] io_input,
  input        io_en,
  input        io_clr,
  input        io_load,
  output [2:0] io_output
);
  reg [2:0] register; // @[Counter.scala 18:25]
  reg [31:0] _RAND_0;
  wire [2:0] _T_1; // @[Counter.scala 21:45]
  assign _T_1 = register + 3'h1; // @[Counter.scala 21:45]
  assign io_output = register; // @[Counter.scala 24:13]
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
  `ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  register = _RAND_0[2:0];
  `endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`endif // SYNTHESIS
  always @(posedge clock) begin
    if (reset) begin
      register <= 3'h0;
    end else if (io_clr) begin
      register <= 3'h0;
    end else if (io_load) begin
      register <= io_input;
    end else if (io_en) begin
      register <= _T_1;
    end
  end
endmodule
