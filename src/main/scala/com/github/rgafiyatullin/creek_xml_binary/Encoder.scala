package com.github.rgafiyatullin.creek_xml_binary

import java.nio.ByteBuffer

import com.github.rgafiyatullin.creek_xml.common.{Attribute, HighLevelEvent}

import scala.collection.immutable.Queue

object Encoder {
  def create(namesCount: Int): Encoder = Encoder(MasterNamesCtx.create(namesCount))
}

final case class Encoder(namesCtx: MasterNamesCtx, events: Queue[StreamEvent] = Queue.empty) {

  private def encodeOpenElement(localName: String, namespace: String, attributes: Seq[Attribute]): (Seq[StreamEvent], Encoder) = {
    val (localNameId, nameEvents1, namesCtx1) = namesCtx.resolveS2I(localName)
    val (namespaceId, nameEvents2, namesCtx2) = namesCtx1.resolveS2I(namespace)

    val openElementStartNameEvents = nameEvents1 ++ nameEvents2
    val openElementStartEvent = StreamEvent.OpenElementStart(namespaceId, localNameId)

    val (attributeEvents, namesCtx3) = attributes.foldLeft((Queue.empty[StreamEvent], namesCtx2)) {
      case ((eventsAccIn, namesCtxIn), Attribute.Unprefixed(attrName, attrValue)) =>
        val (attrNameId, nameEvents, namesCtxOut) = namesCtxIn.resolveS2I(attrName)
        val eventsAccOut = nameEvents
          .foldLeft(eventsAccIn)(_.enqueue(_))
          .enqueue(StreamEvent.ElementUnprefixedAttribute(attrNameId, attrValue))
        (eventsAccOut, namesCtxOut)

      case (asIs, _) =>
        asIs
    }

    ((openElementStartNameEvents :+ openElementStartEvent) ++ attributeEvents, copy(namesCtx = namesCtx3))
  }

  def encodeToStreamEvents(e: HighLevelEvent): (Seq[StreamEvent], Encoder) = e match {
    case HighLevelEvent.ElementOpen(_, _, localName, namespace, attributes) =>
      val (streamEvents, encoderOut) = encodeOpenElement(localName, namespace, attributes)
      (streamEvents ++ Seq(StreamEvent.OpenElementEnd), encoderOut)

    case HighLevelEvent.ElementSelfClosing(_, _, localName, namespace, attributes) =>
      val (streamEvents, encoderOut) = encodeOpenElement(localName, namespace, attributes)
      (streamEvents ++ Seq(StreamEvent.OpenElementEnd, StreamEvent.CloseElement), encoderOut)

    case HighLevelEvent.ElementClose(_, _, _, _) =>
      (Seq(StreamEvent.CloseElement), this)

    case HighLevelEvent.Whitespace(_, whitespace) =>
      (Seq(StreamEvent.PCData(whitespace)), this)

    case HighLevelEvent.PCData(_, text) =>
      (Seq(StreamEvent.PCData(text)), this)

    case HighLevelEvent.CData(_, text) =>
      (Seq(StreamEvent.CData(text)), this)

    case HighLevelEvent.ProcessingInstrutcion(_, _, _) =>
      (Seq(), this)

    case HighLevelEvent.Comment(_, _) =>
      (Seq(), this)
  }


  def encode(e: HighLevelEvent): (Array[Byte], Encoder) = {
    val (events, encoderNext) = encodeToStreamEvents(e)

    val eventsBytes = events.map(_.encode)

    val size = eventsBytes.map(_.length).sum
    val bb = ByteBuffer.allocate(size)
    eventsBytes.foreach(bb.put)
    val dst = Array.fill[Byte](size)(0)
    bb.flip()
    bb.get(dst)
    (dst, encoderNext)
  }

}


