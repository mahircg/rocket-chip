package Top {

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioIPrefetcher extends Bundle() {
  val icache = new ioDCache();
  val mem = new ioDCache().flip()
  val invalidate = Bool(INPUT)
}

class rocketIPrefetcher extends Component() {
  val io = new ioIPrefetcher();
  val pdq = (new queue(REFILL_CYCLES, flushable = true)) { Bits(width = MEM_DATA_BITS) };

  val s_invalid :: s_valid :: s_refilling :: s_req_wait :: s_resp_wait :: s_bad_resp_wait :: Nil = Enum(6) { UFix() };
  val state = Reg(resetVal = s_invalid);

  val demand_miss = io.icache.req_val & io.icache.req_rdy;
  val prefetch_addr = Reg() { UFix(width = io.icache.req_addr.width) };
  when (demand_miss) { prefetch_addr := io.icache.req_addr + UFix(1); }
  
  val addr_match = (prefetch_addr === io.icache.req_addr);
  val hit = (state != s_invalid) & (state != s_req_wait) & addr_match;
  
  io.icache.req_rdy := io.mem.req_rdy;
  val ip_mem_req_rdy  = io.mem.req_rdy & ~(io.icache.req_val & ~hit);
  val ip_mem_resp_val = io.mem.resp_val && io.mem.resp_tag(0).toBool; 
  
  io.mem.req_val  := io.icache.req_val & ~hit | (state === s_req_wait);
  io.mem.req_rw   := Bool(false)
  io.mem.req_tag  := Mux(io.icache.req_val && !hit, UFix(0), UFix(1))
  io.mem.req_addr := Mux(io.mem.req_tag(0).toBool, prefetch_addr, io.icache.req_addr);
  
  val fill_cnt = Reg(resetVal = UFix(0, ceil(log(REFILL_CYCLES)/log(2)).toInt));
  when (ip_mem_resp_val.toBool) { fill_cnt := fill_cnt + UFix(1); }
  val fill_done = (~fill_cnt === UFix(0)) & ip_mem_resp_val;
  
  val forward = Reg(resetVal = Bool(false));
  val forward_cnt = Reg(resetVal = UFix(0, ceil(log(REFILL_CYCLES)/log(2)).toInt));
  when (forward & pdq.io.deq.valid) { forward_cnt := forward_cnt + UFix(1); }
  val forward_done = (~forward_cnt === UFix(0)) & pdq.io.deq.valid;
  forward := (demand_miss & hit | forward & ~forward_done);  

  io.icache.resp_val  := (io.mem.resp_val && !io.mem.resp_tag(0).toBool) || (forward && pdq.io.deq.valid);
  io.icache.resp_data := Mux(forward, pdq.io.deq.bits, io.mem.resp_data);
  
  pdq.io.flush := Reg(demand_miss && !hit || (state === s_bad_resp_wait), resetVal = Bool(false))
  pdq.io.enq.bits := io.mem.resp_data;
  pdq.io.enq.valid  := ip_mem_resp_val.toBool;
  pdq.io.deq.ready  := forward;
  
  switch (state) {
    is (s_invalid) {
      when (demand_miss) { state := s_req_wait; }
    }
    is (s_valid) {
      when (demand_miss | (forward & forward_done)) { state := s_req_wait; }
    }
    is (s_refilling) {
      when (demand_miss & ~addr_match & fill_done.toBool) { state := s_req_wait; }
      .elsewhen (demand_miss & ~addr_match) { state := s_bad_resp_wait; }
      .elsewhen (fill_done.toBool) { state := s_valid; }
    }
    is (s_req_wait) {
      when (ip_mem_req_rdy) { state := s_resp_wait; }
    }
    is (s_resp_wait) {
      when (demand_miss & ~addr_match) { state := s_bad_resp_wait; }
      .elsewhen (ip_mem_resp_val.toBool) { state := s_refilling; }
    }
    is (s_bad_resp_wait) {
      when (fill_done.toBool & ip_mem_resp_val.toBool) { state := s_req_wait; }
    }
  }

  when (io.invalidate) {
    state := s_invalid
  }
}

}
