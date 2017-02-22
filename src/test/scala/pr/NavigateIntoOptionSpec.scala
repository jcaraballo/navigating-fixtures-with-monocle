package pr

import monocle.law.discipline.{IsoTests, PrismTests}
import monocle.macros.GenIso
import monocle.{Iso, Prism}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.typelevel.discipline.scalatest.Discipline

import scalaz.{Equal, Functor}


case class Alpha(text: String = "content")
object Alpha {
  val textI: Iso[Alpha, String] = GenIso[Alpha, String]
}
case class Beta(alpha: Alpha = Alpha())
object Beta {
  val alphaI: Iso[Beta, Alpha] = GenIso[Beta, Alpha]
}

case class Gamma(beta: Option[Beta] = None)

object Gamma {
  val betaI: Iso[Gamma, Option[Beta]] = GenIso[Gamma, Option[Beta]]
  val betaP: Prism[Gamma, Beta] = Prism[Gamma, Beta](_.beta)(a => Gamma(Some(a)))
}

class NavigateIntoOptionSpec extends FunSuite with Discipline {

  implicit val arbitraryAlpha: Arbitrary[Alpha] = Arbitrary(arbitrary[String].map(Alpha.apply))
  implicit val arbitraryBeta: Arbitrary[Beta] = Arbitrary(arbitrary[Alpha].map(Beta.apply))
  implicit val arbitraryGamma: Arbitrary[Gamma] = Arbitrary(arbitrary[Beta].map(b => Gamma(Some(b))))
  implicit val arbitraryAlphaToAlpha: Arbitrary[Alpha => Alpha] = Arbitrary(arbitrary[String => String].map(Alpha.textI.modify))
  implicit val arbitraryBetaToBeta: Arbitrary[Beta => Beta] = Arbitrary(arbitrary[Alpha => Alpha].map(Beta.alphaI.modify))

  implicit val arbitraryOptionBeta: Arbitrary[Option[Beta]] = Arbitrary(Gen.option(arbitrary[Beta]))

  implicit val arbitraryOptionBetaToOptionBeta: Arbitrary[Option[Beta] => Option[Beta]] = Arbitrary(
    arbitrary[Option[String] => Option[String]].map { f =>
      import scalaz.std.option.optionInstance
      val composed = Beta.alphaI composeIso Alpha.textI
      Functor[Option].lift(composed.get) andThen f andThen Functor[Option].lift(composed.reverseGet)
      // or:
      // ((_.map(composed.get)): Option[Beta] => Option[String]) andThen f andThen (_.map(composed.reverseGet))
    }
  )

  implicit val alphaEqual: Equal[Alpha] = Equal.equalA[Alpha]
  implicit val betaEqual: Equal[Beta] = Equal.equalA[Beta]
  implicit val optionBetaEqual: Equal[Option[Beta]] = Equal.equalA[Option[Beta]]
  implicit val gammaEqual: Equal[Gamma] = Equal.equalA[Gamma]

  import scalaz.std.string.stringInstance

  checkAll("Alpha.textI is an Iso", IsoTests(Alpha.textI))
  checkAll("Beta.alphaI is an Iso", IsoTests(Beta.alphaI))
  checkAll("Gamma.betaI is an Iso", IsoTests(Gamma.betaI))
  checkAll("Gamma.betaP is a Prism", PrismTests(Gamma.betaP))

  test("Navigates through Isos"){
    (Beta.alphaI composeIso Alpha.textI).set("foo")(Beta()) shouldBe Beta(Alpha("foo"))
  }

  test("Navigates into Optional") {
    (Gamma.betaP composeIso Beta.alphaI composeIso Alpha.textI).set("foo")(Gamma(None)) shouldBe Gamma(None)
    (Gamma.betaP composeIso Beta.alphaI composeIso Alpha.textI).set("foo")(Gamma(Some(Beta()))) shouldBe Gamma(Some(Beta(Alpha("foo"))))

    (Gamma.betaI.set(Some(Beta())) andThen (Gamma.betaP composeIso Beta.alphaI composeIso Alpha.textI).set("foo")) (Gamma()) shouldBe Gamma(Some(Beta(Alpha("foo"))))
  }
}
