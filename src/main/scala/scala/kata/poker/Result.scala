package scala.kata.poker

/**
  * Created by chengl on 4/12/15.
  */
trait Result

case object Tie extends Result {
  override def toString: String = "Tie."
}

case class Winner(name: String, reason: String) extends Result {
  override def toString: String = s"${name} wins. - with ${reason}"
}