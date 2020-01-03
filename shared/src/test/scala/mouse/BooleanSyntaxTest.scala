package mouse

import cats.syntax.either._
import alleycats.Empty._

class BooleanSyntaxTest extends MouseSuite {

  true.option(1) shouldEqual Option(1)

  false.option(1) shouldEqual Option.empty[Int]

  true.either("error", 1) shouldEqual Either.right(1)

  false.either("error", 1) shouldEqual Either.left("error")

  true.fold("t", "f") shouldEqual "t"
  
  false.fold("t", "f") shouldEqual "f"

  true.valueOrZero(Option(())) shouldEqual Option(())

  false.valueOrZero(Option(())) shouldEqual Option.empty[Unit]

  true.valueOrZero("Yellow") shouldEqual "Yellow"

  false.valueOrZero("Yellow") shouldEqual ""

  true.zeroOrValue("Yellow") shouldEqual ""

  false.zeroOrValue("Yellow") shouldEqual "Yellow"

  true.??("Yellow") shouldEqual "Yellow"

  true.!?("Yellow") shouldEqual ""

  implicit val e: Empty[

  true.valueOrEmpty(Option('a')) shouldEqual Some('a')

  false.valueOrEmpty(Option('a')) shouldEqual None

  true.emptyOrValue(Option('a')) shouldEqual Some('a')

  false.emptyOrValue(Option('a')) shouldEqual None

  true.valueOrPure(Option(1))(2) shouldEqual Some(1)

  false.valueOrPure(Option(1))(2) shouldEqual Some(2)
}
