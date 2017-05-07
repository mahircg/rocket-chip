package rocket

import Chisel._
import uncore._
import open_soc_debug._
import Util._
import cde.{Parameters, Field}


abstract class ASCBundle extends Bundle {
  val iwmc_out = UInt(OUTPUT, 32)
}

class ASCLumpedReleaseBundle(numBlocks: Int) extends ASCBundle {
  val block = UInt(INPUT, numBlocks)
  val status = Bool(OUTPUT)
}

class ASCPreciseReleaseBundle extends ASCBundle

abstract class AtomicSCL1(numBlocks: Int) extends Module{

  private val IWMC = Reg(init = UInt(0, 32))

  def incrementMissCount {
    IWMC := IWMC + 1
  }

  def decrementMissCount {
    IWMC := IWMC - 1
  }

  def readMissCount = IWMC

  def onWriteMiss

}


class ASCLumpedReleaseL1(numBlocks: Int)  extends AtomicSCL1(numBlocks) {
 
  val init_values = Seq.fill(numBlocks) { Bool(false) }
  val io = new ASCLumpedReleaseBundle(numBlocks)
  val mutex_vector = Reg(init = Vec(init_values))
  val current_block = Reg(next = io.block)

  override def onWriteMiss {
    incrementMissCount
  }

  def acquireBlock(block: UInt) {
    val acquire_success = Reg(init = Bool(false))
    if (mutex_vector(current_block) == false) {
      mutex_vector(current_block) := true
      acquire_success := true
    }
    else {
      acquire_success := false
    }
    acquire_success
  }

  io.status := mutex_vector(current_block)
  io.iwmc_out := readMissCount
}

class ASCPreciseReleaseL1(numBlocks: Int)  extends AtomicSCL1(numBlocks) {
  val io = new ASCPreciseReleaseBundle
  override def onWriteMiss {
    incrementMissCount
  }
}
