package com.github.rgafiyatullin.creek_xml_binary

import scala.collection.immutable.Queue

object NamesCtx {
  sealed trait Event
  final case class Add(s: String, i: Int) extends Event
  final case class Remove(s: String, i: Int) extends Event

  def create(size: Int): NamesCtx = NamesCtx(availableIds = (for {i <- 1 to size} yield i).toSet)
}

final case class NamesCtx(availableIds: Set[Int] = Set.empty, usages: Map[Int, Long] = Map.empty, s2i: Map[String, Int] = Map.empty, i2s: Map[Int, String] = Map.empty) {
  def resolveS2I(s: String): (Int, Seq[NamesCtx.Event], NamesCtx) =
    s2i.get(s).fold[(Int, Seq[NamesCtx.Event], NamesCtx)]{
        val (cleanupEvents, ctx1) = if (availableIds.isEmpty) performCleanup else (Queue.empty, this)
        val id = ctx1.availableIds.head
        val ctx2 = ctx1.addResolution(id, s)
        val eventAddResolution = NamesCtx.Add(s, id)

        (id, cleanupEvents.enqueue(eventAddResolution), ctx2)
      } { id =>
        (id, Seq.empty, bumpUsage(id))
      }

  def resolveI2S(i: Int): Option[(String, NamesCtx)] =
    for {
      s <- i2s.get(i)
    }
      yield (s, bumpUsage(i))


  private def gcCount = Math.max((usages.size * 0.1).toInt, 1)

  private def performCleanup: (Queue[NamesCtx.Event], NamesCtx) = {
    val idsToRemove = usages.toSeq.sortBy(_._2).take(gcCount).map(_._1)
    idsToRemove.foldLeft((Queue.empty[NamesCtx.Event], this)) {
      case ((accEvents, accCtx), id) =>
        val str = i2s(id)
        val evt = NamesCtx.Remove(str, id)
        (accEvents.enqueue(evt), accCtx.removeResolution(id, str))
    }
  }

  private def consumeID: (Int, NamesCtx) = {
    val id = availableIds.head
    (id, copy(availableIds = availableIds - id))
  }

  private def removeResolution(id: Int, str: String): NamesCtx = {
    assert(!availableIds.contains(id))
    copy(usages = usages - id, s2i = s2i - str, i2s = i2s - id, availableIds = availableIds + id)
  }

  private def addResolution(id: Int, str: String): NamesCtx = {
    assert(availableIds.contains(id))
    copy(usages = usages + (id -> 1), s2i = s2i + (str -> id), i2s = i2s + (id -> str), availableIds = availableIds - id)
  }

  private def bumpUsage(id: Int): NamesCtx = {
    val oldValueOption = usages.get(id)
    oldValueOption.fold(this)(times => copy(usages = usages + (id -> (times + 1))))
  }
}
