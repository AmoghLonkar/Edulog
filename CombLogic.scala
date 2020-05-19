package comblogic

import java.io.File

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class CombLogic extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val c = Input(UInt(1.W))
    val output = Output(UInt(1.W))
  })

  io.output := (~io.a & io.b) | (~io.b & io.a & io.c)
}

class TestCombLogic(c: CombLogic) extends PeekPokeTester(c){
  poke(c.io.a, 0)
  poke(c.io.b, 1)
  poke(c.io.c, 1)
  step(1)
  expect(c.io.output, 1)

  poke(c.io.a, 1)
  poke(c.io.b, 0)
  poke(c.io.c, 0)
  step(1)
  expect(c.io.output, 0)
}

object Main extends App {
  val works = Driver(() => new CombLogic) {
    c => new TestCombLogic(c)
  }

  assert(works)        // Scala Code: if works == false, will throw an error
  println("SUCCESS!!") // Scala Code: if we get here, our tests passed!

  println(chisel3.Driver.emitVerilog(new CombLogic))

  val f = new File("comblogic.fir")
  chisel3.Driver.dumpFirrtl(chisel3.Driver.elaborate(() => new CombLogic), Option(f))
}