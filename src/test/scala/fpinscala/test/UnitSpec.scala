package fpinscala.test

import org.scalatest._

/**
  * Created by Gnob on 2017. 2. 28..
  */

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors
