package tests

import com.github.rgafiyatullin.creek_xml_binary.{MasterNamesCtx, StreamEvent}
import org.scalatest.{FlatSpec, Matchers}

class MasterNamesCtxTest extends FlatSpec with Matchers {
  val emptyCtx = MasterNamesCtx.create(5)

  def matchEventAdd(s: String): StreamEvent.NameOperation => Boolean = {
      case StreamEvent.NameAdd(_, `s`) => true
      case _ => false
    }

  def matchEventRemove(s: String): StreamEvent.NameOperation => Boolean = {
    case StreamEvent.NameRemove(_, `s`) => true
    case _ => false
  }


  "ctx" should "generate add-event per each new resolution and generate rm-event for least frequently used resolution when full" in {
    val (idA, eventsA, ctxA) = emptyCtx.resolveS2I("a")
    val (idB, eventsB, ctxAB) = ctxA.resolveS2I("b")
    val (idC, eventsC, ctxABC) = ctxAB.resolveS2I("c")
    val (idD, eventsD, ctxABCD) = ctxABC.resolveS2I("d")
    val (idE, eventsE, ctxABCDE) = ctxABCD.resolveS2I("e")
    val (_, _, ctxABCDE1) = ctxABCDE.resolveS2I("a")
    val (_, _, ctxABCDE2) = ctxABCDE1.resolveS2I("b")
    val (_, _, ctxABCDE3) = ctxABCDE2.resolveS2I("c")
    val (_, _, ctxABCDE4) = ctxABCDE3.resolveS2I("d")
    val (idF, eventsF, ctxABCDE5) = ctxABCDE4.resolveS2I("f")

    eventsA.find(matchEventAdd("a")) should not be empty
    eventsB.find(matchEventAdd("b")) should not be empty
    eventsC.find(matchEventAdd("c")) should not be empty
    eventsD.find(matchEventAdd("d")) should not be empty
    eventsE.find(matchEventAdd("e")) should not be empty
    eventsF.find(matchEventAdd("f")) should not be empty
    eventsF.find(matchEventRemove("e")) should not be empty
  }
}
