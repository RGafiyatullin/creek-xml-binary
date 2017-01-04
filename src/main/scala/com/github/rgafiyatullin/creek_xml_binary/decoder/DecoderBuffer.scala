package com.github.rgafiyatullin.creek_xml_binary.decoder

import java.nio.ByteBuffer

import scala.collection.immutable.Queue

object DecoderBuffer {
  def empty: DecoderBuffer = DecoderBuffer()
}

final case class DecoderBuffer(q: Queue[Array[Byte]] = Queue.empty) {
  def put(bytes: Array[Byte]): DecoderBuffer = copy(q = q.enqueue(bytes))

  def get(nBytes: Int): (Option[Array[Byte]], DecoderBuffer) =
    q.headOption match {
      case None =>
        if (nBytes > 0)
          (None, this)
        else
          (Some(Array()), this)
      case Some(chunk) =>
        val q1 = q.drop(1)

        chunk.length compare nBytes match {
          case 0 =>
            (Some(chunk), copy(q = q1))

          case 1 =>
            val result = chunk.slice(0, nBytes)
            val remainder = chunk.slice(nBytes, chunk.length)
            val q2 = remainder +: q1
            (Some(result), copy(q = q2))

          case -1 =>
            val bb = ByteBuffer.allocate(nBytes)
            val q2 = takeChunks(nBytes, bb, q)

            val resultSize = bb.capacity() - bb.remaining()
            val result = Array.fill[Byte](resultSize)(0)
            bb.flip()
            bb.get(result)

            if (result.length == nBytes)
              (Some(result), copy(q = q2))
            else {
              val q3 = result +: q2
              (None, copy(q = q3))
            }
        }
    }

  private def takeChunks(nBytesLeft: Int, bb: ByteBuffer, q: Queue[Array[Byte]]): Queue[Array[Byte]] =
    (nBytesLeft, q.headOption) match {
      case (0, _) => q
      case (_, None) => Queue.empty
      case (n, Some(chunk)) if chunk.length < n =>
        bb.put(chunk)
        takeChunks(n - chunk.length, bb, q.drop(1))

      case (n, Some(chunk)) if chunk.length == n =>
        bb.put(chunk)
        q.drop(1)

      case (n, Some(chunk)) if chunk.length > n =>
        bb.put(chunk.slice(0, n))
        val chunk1 = chunk.slice(n, chunk.length)
        chunk1 +: q.drop(1)
    }


}
