package com.github.rgafiyatullin.creek_xml_binary.decoder

final case class SlaveNamesCtx(i2s: Map[Int, String] = Map.empty, s2i: Map[String, Int] = Map.empty) {
  def addResolution(id: Int, str: String): SlaveNamesCtx = copy(i2s = i2s + (id -> str), s2i = s2i + (str -> id))
  def removeResolution(id: Int, str: String): SlaveNamesCtx = copy(i2s = i2s - id, s2i = s2i - str)

  def resolveI2S(id: Int): Option[String] = i2s.get(id)
  def resolveS2I(str: String): Option[Int] = s2i.get(str)
}
