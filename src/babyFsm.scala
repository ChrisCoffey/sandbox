

abstract class FsmState[T <: Enumeration](val state: T)

object BabyState extends Enumeration{
  type State = Value
  val Happy, Hungry, Sitting, Sleeping = Value
}
case class Baby(name: String, babyState: BabyState.State) extends FsmState(state =  babyState)

trait Event

case class Cried(id: String) extends Event
case class Ate(id: String) extends Event
case class Sat(id: String) extends Event
case class LaidDown(id: String) extends Event

object Baby{

  private def cloneNewState(d: Baby, s: BabyState.State) =
    d.copy(babyState = s)

  def transition(d: Baby, e: Event): Baby = {
    d.babyState match {
      case BabyState.Happy =>
        e match {
          case Cried(x) => cloneNewState(d, BabyState.Hungry)
          case Sat(x) => cloneNewState(d, BabyState.Sitting)
          case _ => d
        }
      case BabyState.Hungry =>
        e match {
          case Ate(x) => cloneNewState(d, BabyState.Happy)
          case _ => d
        }
      case BabyState.Sitting =>
        e match{
          case LaidDown(x) => cloneNewState(d, BabyState.Sleeping)
          case Cried(x) => cloneNewState(d, BabyState.Hungry)
          case _ => d
        }
      case BabyState.Sleeping =>
        e match{
          case Cried(x) => cloneNewState(d, BabyState.Happy)
          case _ => d
        }
      case _ => d
    }
  }

}