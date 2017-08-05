package rocket

import Chisel._
import uncore._
import open_soc_debug._
import Util._
import cde.{Parameters, Field}


abstract class ASCBundle extends Bundle {
  val iwmc_en = Bool(INPUT)
  val iwmc_inc = Bool(INPUT)
  val iwmc_dec = Bool(INPUT)
  val iwmc_out = UInt(OUTPUT, 32)
}

class ASCLumpedReleaseBundle(numMutexes: Int) extends ASCBundle {
  val block = UInt(INPUT, numMutexes)
  val is_acquired = Bool(OUTPUT)
  val acquire_success = Bool(OUTPUT)
}

class ASCPreciseReleaseBundle extends ASCBundle

abstract class AtomicSCL1(numMutexes: Int) extends Module{

  private val IWMC = Reg(init = UInt(0, 32))

  def hashAddr(addr: UInt, offsetBits: UInt, mutexBits: UInt, UInt pAddrBits) {
    val mask = UInt((1 << pAddrBits) - 1, pAddrBits)
    mask := mask << mutexBits
    val idx = (addr >> offsetBits) & mask
  }

  def modifyMissCount(iwmc_en: Bool, iwmc_inc: Bool, iwmc_dec: Bool): UInt = 
  {
    when (iwmc_en) {
      when (iwmc_inc && iwmc_dec) {
        IWMC := IWMC 
      }
      .elsewhen (iwmc_inc) {
        IWMC := IWMC + 1
      }
      .elsewhen (iwmc_dec) {
        IWMC := IWMC - 1
      }
      .otherwise {
        IWMC := IWMC
      }
    }
    return IWMC
  }

  def onWriteMiss

}


class ASCLumpedReleaseL1(numMutexes: Int)  extends AtomicSCL1(numMutexes) {
 
  //val init_values = Seq.fill(numMutexes) { Bool(false) }
  val io = new ASCLumpedReleaseBundle(numMutexes)
  val mutex_vector = Reg(init = UInt(0, numMutexes))
  val current_block = Reg(next = io.block)

  override def onWriteMiss = {
    
  }

  def calcHash(addr: UInt

  def acquireBlock(block: UInt) = {
    val acquire_success = Reg(init = Bool(false))
    if (mutex_vector(current_block) == UInt(0)) {
      mutex_vector(current_block) := 1
      acquire_success := true
      //modifyMissCount(iwmc_en = Bool(true), iwmc_inc = Bool(true), iwmc_dec = Bool(false))
    }
    else {
      acquire_success := false
    }
    acquire_success
  }

  io.acquire_success := acquireBlock(current_block)
  io.is_acquired := mutex_vector(current_block)
  io.iwmc_out := modifyMissCount(iwmc_en = io.iwmc_en, iwmc_inc = io.iwmc_inc, iwmc_dec = io.iwmc_dec)
}

class ASCPreciseReleaseL1(numMutexes: Int)  extends AtomicSCL1(numMutexes) {
  val io = new ASCPreciseReleaseBundle
  override def onWriteMiss {
  }
}
