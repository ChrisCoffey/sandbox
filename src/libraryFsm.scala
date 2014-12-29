
trait DFAState

trait Result
trait Request

trait DFA[T, Input] {
	
	def transition(in: Input)(state: T): (Result, T)

}

trait TranRequest{
  val amount: Int
}

case class Credited(amount: Int) extends Result
case class Debited(amount: Int) extends Result
case class OverdrawApproved(amount: Int) extends Result
case class OverdrawRejected(amount: Int) extends Result

case class ApproveOverdraw(amount: Int) extends Request with TranRequest
case class RejectOverdraw(amount: Int) extends Result with TranRequest
case class Credit(amount: Int) extends Request with TranRequest
case class Debit(amount: Int) extends Result with TranRequest

trait AccountState extends DFAState
case object Open extends AccountState
case object Overdrawn extends AccountState

case class Guarded[A <: AccountState, B <: AccountState](s: A, prev: B)

/**
 * A simple case class representing a bank acocunt.
 *
 */
case class BankAccount(balance: Int, state: AccountState)

object BankAccount extends DFA[BankAccoun, TransactionRequest]{

	def transition(in: Input)(state: T): (Result, T)={
 		state.state match{
      case Open => {
      in match {
        case Credit(x) => (Credited(x), BankAccount(balancePostTransaction(state, in), Open))
        case Debit(x) if balancePostTransaction(state, in) >=0 => (Debited(x), BankAccount(balancePostTransaction(state, in), Open))
        case Debit(x) => (Debited(x), BankAccount(state.balance , Guarded(Overdrawn, Open)))
       }
      }
        case Guarded(Overdrawn, prev) => in match {
          // Allow the guarded transition to proceed
          case ApproveOverdraw(amt) => (OverdrawApproved(amt), BankAccount(balancePostTransaction(state, in), Overdrawn)
            // Reject the guarded transition and move back to prior state
            case RejectOverdraw(amt) => (OverdrawRejected(amt), state.copy( state = prev))
        }
        case Overdrawn => {
          case Credit(x) => ???
        } 
  }
  }

  private def balancePostTransaction(acct: BankAccount, tran: TranRequest): Int = {
    acct.balance + tran.amount 
  }


}
