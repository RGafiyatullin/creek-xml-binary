package tests

import java.nio.ByteBuffer

import com.github.rgafiyatullin.creek_xml.common.{Attribute, HighLevelEvent, QName}
import com.github.rgafiyatullin.creek_xml.dom.{CData, Element, NodeBuilder, PCData}
import com.github.rgafiyatullin.creek_xml_binary.StreamEvent
import com.github.rgafiyatullin.creek_xml_binary.decoder.Decoder
import com.github.rgafiyatullin.creek_xml_binary.encoder.Encoder
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class EncoderAndDecoderTest extends FlatSpec with Matchers {

  @tailrec
  final def keepDecodingStreamEvents(acc: Queue[StreamEvent], decoder0: Decoder): Seq[StreamEvent] =
    decoder0.getStreamEvent match {
      case (Some(event), decoder1) =>
        keepDecodingStreamEvents(acc.enqueue(event), decoder1)
      case (None, _) =>
        acc
    }


  @tailrec
  final def keepDecodingHLEvents(acc: Queue[HighLevelEvent], decoder0: Decoder): Seq[HighLevelEvent] =
    decoder0.getHighLevelEvent match {
      case (Some(event), decoder1) =>
        keepDecodingHLEvents(acc.enqueue(event), decoder1)

      case (None, _) =>
        acc
    }

  "encoder" should "encode" in {
    val ns1 = "one:ns:1"
    val ns2 = "two:ns:2"
    val ln1 = "local-name-1"
    val ln2 = "local-name-2"
    val attr1 = Attribute.Unprefixed("from", "1@a/laptop")
    val attr2 = Attribute.Unprefixed("to", "2@b/mobile")

    val qn11 = QName(ns1, ln1)
    val qn12 = QName(ns1, ln2)
    val qn21 = QName(ns2, ln1)
    val qn22 = QName(ns2, ln2)

    val xml = Element(qn11, Seq(attr1, attr2), Seq(
      Element(qn12, Seq(), Seq(
        CData("hello there")
      )),
      Element(qn21, Seq(), Seq(
        Element(qn22, Seq(attr1, attr2), Seq(
          PCData("the angel of my nightmare")
        ))
      ))
    ))

    val events = xml.toEvents

    val encoder0 = Encoder.create(64)
    val bb = ByteBuffer.allocate(2048)
    events.foldLeft(encoder0) {
      case (encIn, event) =>
        val (bytes, encOut) = encIn.encode(event)
        bb.put(bytes)
        encOut
    }

    val encodedBytes = Array.fill[Byte](bb.capacity() - bb.remaining())(0)
    bb.flip()
    bb.get(encodedBytes)

    print("Array: [")
    for {b <- encodedBytes.map(_ & 0xff)}
      print("%d ".format(b))
    println("]")

    val decoder0 = Decoder.create()
    val decoder1 = decoder0.putBytes(encodedBytes)

    val decodedStreamEvents = keepDecodingStreamEvents(Queue.empty, decoder1)
    println("Decoded stream events: [")
    for {e <- decodedStreamEvents}
      println("- %s".format(e))
    println("]")

    val decodedHighLevelEvents = keepDecodingHLEvents(Queue.empty, decoder1)

    println("Decoded HL events: [")
    for {e <- decodedHighLevelEvents}
      println("- %s".format(e))
    println("]")


    decodedHighLevelEvents.foldLeft(NodeBuilder.empty)(_.in(_)).nodeOption should contain (xml)
  }

  "router error reply" should "encode and decode" in {
    /*
      <iq id='route:793a6262-4d42-431d-9c89-ad758fe9802d' type='error' xmlns='jabber:client'>
        <error xmlns='jabber:client'>
          <undefined-condition xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
          <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'><![CDATA[router: no action succeeded]]></text>
          <error-condition xmlns='http://wargaming.net/xmpp/router-service#condition'>
            <report xmlns='http://wargaming.net/xmpp/router-service#condition'><![CDATA[	To-JID: 'wot-clans@$muc-sync.xmppcs.dev'\n	Stanza: [ns: jabber:client; ncn: message]\n	Matched rule:\n		catch_all_case = 'true'\n]]></report>
          </error-condition>
        </error>
      </iq>
     */
    val xml = CData("\tTo-JID: 'wot-clans@$muc-sync.xmppcs.dev'\n\tStanza: [ns: jabber:client; ncn: message]\n\tMatched rule:\n\t\tcatch_all_case = 'true'\n")
    val events = xml.toEvents
    val encoder0 = Encoder.create(64)
    val bb = ByteBuffer.allocate(2048)
    events.foldLeft(encoder0) {
      case (encIn, event) =>
        val (bytes, encOut) = encIn.encode(event)
        bb.put(bytes)
        encOut
    }
    val encodedBytes = Array.fill[Byte](bb.capacity() - bb.remaining())(0)
    bb.flip()
    bb.get(encodedBytes)

    print("Array: [")
    for (b <- encodedBytes.map(_ & 0xff))
      println("- %d".format(b))
    print("]")

    val decoder0 = Decoder.create()
    val decoder1 = decoder0.putBytes(encodedBytes)

    val decodedStreamEvents = keepDecodingStreamEvents(Queue.empty, decoder1)
    println("Decoded stream events: [")
    for {e <- decodedStreamEvents}
      println("- %s".format(e))
    println("]")

    val decodedHighLevelEvents = keepDecodingHLEvents(Queue.empty, decoder1)

    println("Decoded HL events: [")
    for {e <- decodedHighLevelEvents}
      println("- %s".format(e))
    println("]")


    decodedHighLevelEvents.foldLeft(NodeBuilder.empty)(_.in(_)).nodeOption should contain (xml)
  }
}
