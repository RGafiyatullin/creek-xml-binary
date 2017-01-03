package tests

import java.nio.ByteBuffer

import com.github.rgafiyatullin.creek_xml.common.{Attribute, QName}
import com.github.rgafiyatullin.creek_xml.dom.{CData, Element, PCData}
import com.github.rgafiyatullin.creek_xml_binary.Encoder
import org.scalatest.{FlatSpec, Matchers}

class EncoderTest extends FlatSpec with Matchers {
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
    val bb = ByteBuffer.allocate(1024)
    events.foldLeft(encoder0) {
      case (encIn, event) =>
        val (bytes, encOut) = encIn.encode(event)
        bb.put(bytes)
        encOut
    }

    val dst = Array.fill[Byte](bb.capacity() - bb.remaining())(0)
    bb.flip()
    bb.get(dst)

    print("Array: [")
    for {b <- dst}
      print("%d ".format(b))

    println("]")

  }
}
