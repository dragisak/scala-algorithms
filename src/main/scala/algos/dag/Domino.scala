package algos.dag

case class Memory(bytes: Int)

sealed trait MemoryFormat
object MemoryFormat {
  case object M  extends MemoryFormat
  case object Mi extends MemoryFormat
}

object Memory {

  private val mregex  = """(\d+)\s*([M|Mi])""".r
  private val miregex = """(\d+)\s*Mi""".r

  def parse(str: String): Option[Memory] = str match {
    case miregex(v) => Some(Memory(v.toInt * 1048576))
    case mregex(v)  => Some(Memory(v.toInt * 1000000))
    case _          => None
  }

  def toString(memory: Memory, format: MemoryFormat): String = format match {
    case MemoryFormat.M  => s"${memory.bytes / 1000000} M"
    case MemoryFormat.Mi => s"${memory.bytes / 1048576} Mi"
  }

  implicit def memoryPrint(implicit F: MemoryFormat): Print[Memory] = new Print[Memory] {
    override def toString(t: Memory): String = Memory.toString(t, F)
  }

}

case class Cpus(millis: Double)

object Cpus {

  private val regexm = """(\d+)\s*m""".r
  private val regex  = """(\d+)""".r

  def parse(s: String): Option[Cpus] = s match {
    case regexm(v) => Some(Cpus(v.toInt))
    case regex(v)  => Some(Cpus(v.toInt * 1000))
    case _         => None
  }

  implicit val cpusPrint: Print[Cpus] = new Print[Cpus] {
    override def toString(t: Cpus): String = s"${t.millis} m"
  }
}

trait Print[T] {
  def toString(t: T): String
}

object Print {

  implicit class PrintSyntax[T](t: T) {
    def print(implicit P: Print[T]): String = P.toString(t)
  }

}
