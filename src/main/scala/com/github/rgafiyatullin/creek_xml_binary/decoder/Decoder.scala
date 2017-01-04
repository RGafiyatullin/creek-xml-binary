package com.github.rgafiyatullin.creek_xml_binary.decoder

import com.github.rgafiyatullin.creek_xml.common.HighLevelEvent.{CData, ElementClose, ElementOpen, PCData}
import com.github.rgafiyatullin.creek_xml.common.{Attribute, HighLevelEvent, Position, QName}
import com.github.rgafiyatullin.creek_xml_binary.StreamEvent

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Decoder {
  private val emptyPosition = Position.withoutPosition

  def create(): Decoder = Decoder()

  sealed trait HighLevelState {
    def consumeStreamEvent(se: StreamEvent): Either[HighLevelState, (HighLevelEvent, HighLevelState)]
  }
  val HLSEmpty = HLSOutside(List(), SlaveNamesCtx())

  final case class HLSOutside(qNameStack: List[QName], namesCtx: SlaveNamesCtx) extends HighLevelState {
    override def consumeStreamEvent(se: StreamEvent): Either[HighLevelState, (HighLevelEvent, HighLevelState)] =
      se match {
        case StreamEvent.NameAdd(id, name) =>
          Left(copy(namesCtx = namesCtx.addResolution(id, name)))

        case StreamEvent.NameRemove(id, name) =>
          Left(copy(namesCtx = namesCtx.removeResolution(id, name)))

        case StreamEvent.CData(value) =>
          Right(CData(emptyPosition, value), this)

        case StreamEvent.PCData(value) =>
          Right(PCData(emptyPosition, value), this)

        case StreamEvent.CloseElement =>
          val qn = qNameStack.head
          val qNameStackNext = qNameStack.tail

          Right(ElementClose(emptyPosition, "", qn.localName, qn.ns), copy(qNameStack = qNameStackNext))

        case StreamEvent.OpenElementStart(nsId, lnId) =>
          val qnOption = for {
              ln <- namesCtx.resolveI2S(lnId)
              ns <- namesCtx.resolveI2S(nsId)
            }
              yield QName(ns, ln)

          qnOption match {
            case None =>
              assert(false)
              Left(this)

            case Some(qn) =>
              Left(HLSOpenning(qNameStack, qn, Queue.empty, namesCtx))
          }

        case _ =>
          assert(false)
          Left(this)
      }
  }
  final case class HLSOpenning(qNameStack: List[QName], qName: QName, attributes: Queue[Attribute], namesCtx: SlaveNamesCtx) extends HighLevelState {
    override def consumeStreamEvent(se: StreamEvent): Either[HighLevelState, (HighLevelEvent, HighLevelState)] =
      se match {
        case StreamEvent.NameAdd(id, name) =>
          Left(copy(namesCtx = namesCtx.addResolution(id, name)))

        case StreamEvent.NameRemove(id, name) =>
          Left(copy(namesCtx = namesCtx.removeResolution(id, name)))

        case StreamEvent.OpenElementEnd =>
          Right(ElementOpen(emptyPosition, "", qName.localName, qName.ns, attributes), HLSOutside(qName :: qNameStack, namesCtx))

        case StreamEvent.ElementUnprefixedAttribute(nameId, value) =>
          namesCtx.resolveI2S(nameId) match {
            case Some(name) =>
              val attribute = Attribute.Unprefixed(name, value)
              Left(copy(attributes = attributes.enqueue(attribute)))

            case None =>
              assert(false)
              Left(this)
          }

        case _ =>
          assert(false)
          Left(this)
      }
  }
}

final case class Decoder(buffer: DecoderBuffer = DecoderBuffer.empty, highLevelState: Decoder.HighLevelState = Decoder.HLSEmpty) {
  private object util {
    def bigEndianInt(a: Seq[Byte]): Int =
      a.foldLeft[(Int, Int)](0, 1) {
        case ((acc, mult), byte) =>
          (acc + mult * byte, mult * 256)
      }._1
  }

  def putBytes(bytes: Array[Byte]): Decoder =
    copy(buffer = buffer.put(bytes))

  def getEventBytes: (Option[(Int, Array[Byte])], Decoder) =
    buffer.get(4) match {
      case (None, bufferOut) =>
        (None, copy(buffer = bufferOut))

      case (Some(Array(szLo, szHi, tidLo, tidHi)), buffer1) =>
        val size = util.bigEndianInt(Seq(szLo, szHi))
        val typeId = util.bigEndianInt(Seq(tidLo, tidHi))
        buffer1.get(size) match {
          case (None, _) => (None, this)
          case (Some(payloadBytes), bufferOut) =>
            (Some(typeId, payloadBytes), copy(buffer = bufferOut))
        }
    }

  def getStreamEvent: (Option[StreamEvent], Decoder) =
    getEventBytes match {
      case (None, decoder) => (None, decoder)
      case (Some((tid, bytes)), decoder) =>
        val maybeEvent = StreamEvent.decode(tid, bytes)
        assert(maybeEvent.isDefined)
        (maybeEvent, decoder)
    }

  @tailrec
  def getHighLevelEvent: (Option[HighLevelEvent], Decoder) =
    getStreamEvent match {
      case (None, decoder) =>
        (None, decoder)

      case (Some(se), decoder) =>
        decoder.highLevelState.consumeStreamEvent(se) match {
          case Left(hls) =>
            decoder.copy(highLevelState = hls).getHighLevelEvent
          case Right((hle, hls)) =>
            (Some(hle), decoder.copy(highLevelState = hls))
        }
    }
}
