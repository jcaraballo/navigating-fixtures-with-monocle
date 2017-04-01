package pr

import monocle.law.discipline.{IsoTests, PrismTests}
import monocle.macros.{GenIso, GenLens}
import monocle.{Iso, Lens, Optional, Prism}
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

case class Outer(beta: Option[Beta], text: String)
object Outer {
  val betaL: Lens[Outer, Option[Beta]] = GenLens[Outer](_.beta)
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

  test("Navigates into optional unique child and beyond") {
    val navigateToText: Prism[Gamma, String] = Gamma.betaP composeIso Beta.alphaI composeIso Alpha.textI

    // navigateToText allows us to alter the element deep down...
    navigateToText.set("foo")(Gamma(Some(Beta()))) shouldBe Gamma(Some(Beta(Alpha("foo"))))
    // but by itself it won't do anything if anything intermediate in the descending path is a None :(
    navigateToText.set("foo")(Gamma(None)) shouldBe Gamma(None)

    // So we make it work by turning any intermediate None in the descending path into an appropriate default value and then we can use
    // navigateToText to alter the element deep down
    (Gamma.betaI.modify(_.orElse(Some(Beta()))) andThen navigateToText.set("foo")) (Gamma()) shouldBe Gamma(Some(Beta(Alpha("foo"))))
    (Gamma.betaI.modify(_.orElse(Some(Beta()))) andThen navigateToText.modify(_ + "_foo")) (Gamma(Some(Beta(Alpha("content"))))) shouldBe Gamma(Some(Beta(Alpha("content_foo"))))
    (Gamma.betaI.modify(_.orElse(Some(Beta()))) andThen navigateToText.modify(_ + "_foo")) (Gamma(Some(Beta(Alpha("text"))))) shouldBe Gamma(Some(Beta(Alpha("text_foo"))))


    // Julien's suggestion works awesomely for this example
    import scalaz.std.option.optionInstance

    val navigateToText2: Prism[Gamma, Option[String]] =
      Gamma.betaI composePrism Beta.alphaI.asPrism.below[Option] composePrism Alpha.textI.asPrism.below[Option]

    // when we want to set the inner value regardless:
    navigateToText2.set(Some("foo"))(Gamma(Some(Beta()))) shouldBe Gamma(Some(Beta(Alpha("foo"))))
    navigateToText2.set(Some("foo"))(Gamma(None)) shouldBe Gamma(Some(Beta(Alpha("foo"))))

    // when we want to modify the inner value we also have to provide a default value in case it is not defined ("" in this case)
    navigateToText2.modify(_.orElse(Some("")).map(_ + "_foo"))(Gamma(Some(Beta(Alpha("content"))))) shouldBe Gamma(Some(Beta(Alpha("content_foo"))))
    navigateToText2.modify(_.orElse(Some("")).map(_ + "_foo"))(Gamma(None)) shouldBe Gamma(Some(Beta(Alpha("_foo"))))
  }

  // And when the parent with the optional child has other children it also works on a lens
  test("Navigates from a parent into an optional attribute amongst other ones and beyond"){

    import scalaz.std.option.optionInstance

    val navigateToInnerText: Optional[Outer, Option[String]] =
      Outer.betaL composePrism Beta.alphaI.asPrism.below[Option] composePrism Alpha.textI.asPrism.below[Option]

    navigateToInnerText.set(Some("foo"))(Outer(None, "Hi")) shouldBe Outer(Some(Beta(Alpha("foo"))), "Hi")
    navigateToInnerText.set(Some("foo"))(Outer(Some(Beta(Alpha("content"))), "Hi")) shouldBe Outer(Some(Beta(Alpha("foo"))), "Hi")

    navigateToInnerText.modify(_.orElse(Some("")).map(_ + "_foo"))(Outer(None, "Hi")) shouldBe Outer(Some(Beta(Alpha("_foo"))), "Hi")
    navigateToInnerText.modify(_.orElse(Some("")).map(_ + "_foo"))(Outer(Some(Beta(Alpha("content"))), "Hi")) shouldBe Outer(Some(Beta(Alpha("content_foo"))), "Hi")
  }
}
