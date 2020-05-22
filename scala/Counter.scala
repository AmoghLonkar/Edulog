package counter

import java.io.File

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class Counter extends Module{
  val io = IO(new Bundle {
    val input        = Input(UInt(3.W))
    val en            = Input(Bool())
    val clr           = Input(Bool())
    val load          = Input(Bool())
    val output        = Output(UInt(3.W))
  })

  val register = RegInit(0.U(3.W))
  when(io.clr){register := 0.U}
    .elsewhen(io.load) { register := io.input  }
    .elsewhen(io.en) { register := register + 1.U  }
    .otherwise  { register := register  }

  io.output := register
}

class TestCounter(c: Counter) extends PeekPokeTester(c){
  poke(c.io.input, 5)
  poke(c.io.clr, true)
  poke(c.io.load, false)
  poke(c.io.en, false)
  step(1)
  expect(c.io.output, 0)

  poke(c.io.input, 5)
  poke(c.io.clr, false)
  poke(c.io.load, true)
  poke(c.io.en, false)
  step(1)
  expect(c.io.output, 5)

  poke(c.io.input, 5)
  poke(c.io.clr, false)
  poke(c.io.load, false)
  poke(c.io.en, true)
  step(1)
  expect(c.io.output, 6)
}

object Main extends App {
  val works = Driver(() => new Counter) {
    c => new TestCounter(c)
  }

  assert(works)        // Scala Code: if works == false, will throw an error
  println("SUCCESS!!") // Scala Code: if we get here, our tests passed!

  println(chisel3.Driver.emitVerilog(new Counter))

  val f = new File("counter.fir")
  chisel3.Driver.dumpFirrtl(chisel3.Driver.elaborate(() => new Counter), Option(f))
}