// package azuremips.core.cp0

// import spinal.core._
// import spinal.lib._

// case class CpReg(
//   addr : Int = 0,
//   sel  : Int = 0
// ) extends Bundle {
//   val data = Reg(UInt(32 bits))
//   val mask = UInt(32 bits)

//   def maskedData(data : UInt) : UInt = {
//     data & mask | this.data & ~mask
//   }

//   def update(data : UInt) : Unit = {
//     this.data := maskedData(data)
//   }

//   def init(data : UInt) : Unit = {
//     this.data.init(data)
//   }
// }

// object CpReg {
//   def apply(addr : Int, sel : Int, mask : Int, init : Int) : CpReg = {
//     val r = CpReg(addr, sel)
//     r.data := init
//     r.mask := mask
//     r
//   }

//   def apply(addr : Int, sel : Int, mask : Int) : CpReg = {
//     CpReg(addr, sel, mask, 0)
//   }
// }

// object Cp0Regs{
//   private val nameMap : Map[String, (Int, Int)] = Map {
//     "Index"    -> (0, 0)
//     "Random"   -> (1, 0)
//     "EntryLo0" -> (2, 0)
//     "EntryLo1" -> (3, 0)
//     "Context"  -> (4, 0)
//     "PageMask" -> (5, 0)
//     "Wired"    -> (6, 0)
//     "BadVAddr" -> (8, 0)
//     "Count"    -> (9, 0)
//     "EntryHi"  -> (10, 0)
//     "Compare"  -> (11, 0)
//     "Status"   -> (12, 0)
//     "Cause"    -> (13, 0)
//     "EPC"      -> (14, 0)
//     "PRId"     -> (15, 0)
//     "Config"   -> (16, 0)
//     "Config1"  -> (16, 1)
//     "TagLo"    -> (28, 0)
//     "TagHi"    -> (29, 0)
//     "ErrorEPC" -> (30, 0)
//   }

//   private val cp0Regs : Map[(Int, Int), CpReg] = Map {
//     for ((name, (addr, sel)) <- nameMap) yield {
//       (addr, sel) -> CpReg(addr, sel)
//     }
//   }

//   def apply(name : String) : CpReg = {
//     cp0Regs(nameMap(name))
//   }
// }
