import selfassembly.examples.pickling._
import binary._

import org.evactor.model.events.DataEvent
import scala.util.Random
import java.io._
import scala.reflect.runtime.{universe => ru}

object EvactorPicklingBench extends Benchmark {
  val time: Int = System.currentTimeMillis.toInt
  val size = System.getProperty("size").toInt

  def run() {
    // random events
    val evts = for (i <- 1 to size) yield
      DataEvent("event" + i, time + Random.nextInt(100), Random.nextString(5))

    val pickles = for (evt <- evts) yield
      evt.pickle

    var i = 0
    while (i < size) {
      pickles(i).unpickle[DataEvent]
      i += 1
    }
  }
}
