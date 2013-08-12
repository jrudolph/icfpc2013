package net.virtualvoid

import java.math.BigInteger

package object program {
  def sd = {
    Client.shutdown()
    sys.exit()
  }

  implicit def seqLong2seqString(ls: Seq[Long]): Seq[String] =
    ls.map(_.formatted("0x%X"))

  def parseLong(str: String): Long = new BigInteger(str, 16).longValue()
}
