package com.github.rgafiyatullin.creek_xml_binary

import java.nio.ByteBuffer

sealed trait StreamEvent {
  import StreamEvent._

  def typeId: Byte = this match {
    case _: NameAdd => 1
    case _: NameRemove => 2
    case _: OpenElementStart => 3
    case OpenElementEnd => 4
    case CloseElement => 5
    case _: ElementUnprefixedAttribute => 6
    case _: CData => 7
    case _: PCData => 8
  }

  def genEncode(bs: Seq[Array[Byte]]): Array[Byte] = {
    val size = bs.map(_.length).sum
    val bb = ByteBuffer
      .allocate(size + 4)
      .put(encoding.twoByteInt(typeId))
      .put(encoding.twoByteInt(size))

    bs.foreach(bb.put)

    val dst = Array.fill[Byte](bb.capacity() - bb.remaining())(0)

    bb.flip()
    bb.get(dst)
    dst
  }

  def encode: Array[Byte] =
    this match {
      case NameAdd(id, str) =>
        genEncode(Seq(encoding.twoByteInt(id), encoding.lenPrefixedStr(str)))

      case NameRemove(id, str) =>
        genEncode(Seq(encoding.twoByteInt(id), encoding.lenPrefixedStr(str)))

      case OpenElementStart(nsId, localNameId) =>
        genEncode(Seq(encoding.twoByteInt(nsId), encoding.twoByteInt(localNameId)))

      case OpenElementEnd =>
        genEncode(Seq())

      case CloseElement =>
        genEncode(Seq())

      case ElementUnprefixedAttribute(nameId, value) =>
        genEncode(Seq(encoding.twoByteInt(nameId), encoding.lenPrefixedStr(value)))

      case CData(text) =>
        genEncode(Seq(encoding.lenPrefixedStr(text)))

      case PCData(text) =>
        genEncode(Seq(encoding.lenPrefixedStr(text)))
    }
}

object StreamEvent {
  private object encoding {
    private val csUtf8 = java.nio.charset.Charset.forName("UTF-8")

    def twoByteInt(i: Int): Array[Byte] = {
      val lo = (i % 256).toByte
      val hi = ((i / 256) % 256).toByte
      Array(lo, hi)
    }
    def lenPrefixedStr(s: String): Array[Byte] = {
      val bytes = s.getBytes(csUtf8)
      val len = bytes.length
      val bb = ByteBuffer.allocate(len + 2)
      bb.put(twoByteInt(len)).put(bytes)
      val dst = Array.fill[Byte](len + 2)(0)
      bb.flip()
      bb.get(dst)
      dst
    }
  }


  sealed trait NameOperation extends StreamEvent

  final case class NameAdd(id: Int, name: String) extends NameOperation
  final case class NameRemove(id: Int, name: String) extends NameOperation

  final case class OpenElementStart(nsId: Int, localNameId: Int) extends StreamEvent
  final case class ElementUnprefixedAttribute(nameId: Int, value: String) extends StreamEvent
  case object OpenElementEnd extends StreamEvent
  case object CloseElement extends StreamEvent
  final case class CData(value: String) extends StreamEvent
  final case class PCData(value: String) extends StreamEvent
}
