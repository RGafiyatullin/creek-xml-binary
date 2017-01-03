package tests

import com.github.rgafiyatullin.creek_xml_binary.DecoderBuffer
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Queue

class DecoderBufferTest extends FlatSpec with Matchers {
  def a(first: Int, last: Int): Array[Byte] = (for {i <- first to last} yield i.toByte).toArray

  "decoder buffer" should "accept chunks and return slices of requested sizes" in {
    val db0 = DecoderBuffer.empty
    val chunksIn =
      for {i <- 0 until 10}
        yield (for {j <- 0 until 10} yield (i * 10 + j).toByte).toArray

    val db1 = chunksIn.foldLeft(db0)(_.put(_))

    val (co1, db2) = db1.get(5)
    co1 should contain (a(0,4))
    val (co2, db3) = db2.get(5)
    co2 should contain (a(5,9))
    val (co3, db4) = db3.get(15)
    co3 should contain (a(10,24))
    val (co4, db5) = db4.get(15)
    co4 should contain(a(25,39))
    val (co5ok, db6empty) = db5.get(60)
    val (co5underrun, db6flat) = db5.get(61)

    co5ok should contain(a(40, 99))
    co5underrun should be (None)
    db6empty.q should be(Queue.empty)
    db6flat.q.size should be(1)
    db6flat.q.headOption should contain(a(40,99))
  }

}
