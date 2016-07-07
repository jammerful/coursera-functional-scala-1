/**
  * Created by maddog on 6/16/16.
  */

package idealized.scala

abstract class Boolean {
  // cond.ifThenElse(te, ee) = if(cond) te else ee
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Boolean): Boolean = ifThenElse(x, false)
  def || (x: => Boolean): Boolean = ifThenElse(true, x)
  def unary_! : Boolean = ifThenElse(false, true)

  def == (x: => Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: => Boolean): Boolean = ifThenElse(x.unary_!, x)
  def < (x: => Boolean): Boolean = ifThenElse(false,x)
}

object false extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = e

}

  object true extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = t
}