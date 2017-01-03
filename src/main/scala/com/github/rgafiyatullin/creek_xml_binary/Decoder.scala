package com.github.rgafiyatullin.creek_xml_binary

import scala.collection.immutable.Queue

object Decoder {
  def create(): Decoder = Decoder()
}

final case class Decoder(namesCtx: SlaveNamesCtx = SlaveNamesCtx(), input: Queue[Array[Byte]] = Queue.empty) {

}
