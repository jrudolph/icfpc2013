package net.virtualvoid

package object program {
  def sd = {
    Client.shutdown()
    sys.exit()
  }

  implicit def seqLong2seqString(ls: Seq[Long]): Seq[String] =
    ls.map(_.formatted("0x%X"))
}
