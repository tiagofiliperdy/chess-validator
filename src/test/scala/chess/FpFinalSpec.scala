package chess

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

trait FpFinalSpec
    extends AnyFunSuite
    with Generators
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with Configuration
    with FunSuiteDiscipline
