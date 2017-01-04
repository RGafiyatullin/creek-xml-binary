package com.github.rgafiyatullin.creek_xml_binary

import java.nio.ByteBuffer

import scala.util.Try

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
      .put(encoding.twoByteInt(size))
      .put(encoding.twoByteInt(typeId))

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

  def decode(tid: Int, bytes: Array[Byte]): Option[StreamEvent] =
    tid match {
      case 1 => NameAdd.decode(bytes)
      case 2 => NameRemove.decode(bytes)
      case 3 => OpenElementStart.decode(bytes)
      case 4 => OpenElementEnd.decode(bytes)
      case 5 => CloseElement.decode(bytes)
      case 6 => ElementUnprefixedAttribute.decode(bytes)
      case 7 => CData.decode(bytes)
      case 8 => PCData.decode(bytes)
    }

  private val csUtf8 = java.nio.charset.Charset.forName("UTF-8")

  private object encoding {
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

  private object decoding {
    def lenPrefixedStr(bytes: Array[Byte]): Option[(String, Array[Byte])] =
      for {
        (size, bytes1) <- twoByteInt(bytes)
        (strBytes, bytes2) <-
          if (bytes1.length >= size)
            Some(bytes1.slice(0, size), bytes1.slice(size, bytes1.length))
          else
            None
        str <- Try(new String(strBytes, csUtf8)).toOption
      }
        yield (str, bytes2)

    def twoByteInt(bytes: Array[Byte]): Option[(Int, Array[Byte])] =
      if (bytes.length >= 2) {
        val lo = bytes(0)
        val hi = bytes(1)
        val remainder = bytes.slice(2, bytes.length)

        Some(lo + hi * 256, remainder)
      }
      else None
  }


  sealed trait NameOperation extends StreamEvent

  final case class NameAdd(id: Int, name: String) extends NameOperation
  object NameAdd {
    def decode(bytes: Array[Byte]): Option[NameAdd] =
      for {
        (id, bytes1) <- decoding.twoByteInt(bytes)
        (name, _) <- decoding.lenPrefixedStr(bytes1)
      }
        yield NameAdd(id, name)
  }

  final case class NameRemove(id: Int, name: String) extends NameOperation
  object NameRemove {
    def decode(bytes: Array[Byte]): Option[NameRemove] =
      for {
        (id, bytes1) <- decoding.twoByteInt(bytes)
        (name, _) <- decoding.lenPrefixedStr(bytes1)
      }
        yield NameRemove(id, name)
  }

  final case class OpenElementStart(nsId: Int, localNameId: Int) extends StreamEvent
  object OpenElementStart {
    def decode(bytes: Array[Byte]): Option[OpenElementStart] =
      for {
        (nsId, bytes1) <- decoding.twoByteInt(bytes)
        (localNameId, _) <- decoding.twoByteInt(bytes1)
      }
        yield OpenElementStart(nsId, localNameId)
  }

  final case class ElementUnprefixedAttribute(nameId: Int, value: String) extends StreamEvent
  object ElementUnprefixedAttribute {
    def decode(bytes: Array[Byte]): Option[ElementUnprefixedAttribute] =
      for {
        (nameId, bytes1) <- decoding.twoByteInt(bytes)
        (value, _) <- decoding.lenPrefixedStr(bytes1)
      }
        yield ElementUnprefixedAttribute(nameId, value)
  }

  case object OpenElementEnd extends StreamEvent {
    def decode(bytes: Array[Byte]): Option[this.type] = Some(this)
  }

  case object CloseElement extends StreamEvent {
    def decode(bytes: Array[Byte]): Option[this.type] = Some(this)
  }

  final case class CData(value: String) extends StreamEvent
  object CData {
    def decode(bytes: Array[Byte]): Option[CData] =
      for {
        (value, _) <- decoding.lenPrefixedStr(bytes)
      }
        yield CData(value)
  }

  final case class PCData(value: String) extends StreamEvent
  object PCData {
    def decode(bytes: Array[Byte]): Option[PCData] =
      for {
        (value, _) <- decoding.lenPrefixedStr(bytes)
      }
        yield PCData(value)
  }
}
