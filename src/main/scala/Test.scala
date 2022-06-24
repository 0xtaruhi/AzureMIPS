import spinal.core._
import spinal.lib._

class Test extends Component {
  val io = new Bundle {
    val a = in UInt(8 bits)
    val b = in UInt(8 bits)
    val c = out UInt(8 bits)
  }
  io.c := io.a & io.b
}

object Test {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Test)
  }
}