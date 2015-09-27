import org.scalatest.{Matchers, FunSuite}
import shapeless._, syntax.singleton._, record._, ops.hlist._

class AlignSpec extends FunSuite with Matchers {

  test("order is different, object are different by using == form scala") {
    val foo = ("age" ->> 34) :: ("name" ->> "Jane") :: HNil
    val bar = ("name" ->> "Jane") :: ("age" ->> 34) :: HNil

    foo should not equal bar
  }

  test("order is different, object are same if we are using align from shapeless") {
    def permutatedEqual[R1 <: HList, R2 <: HList](
                        r1: R1, r2: R2
                        )(implicit
                          align: Align[R1, R2]
                        ): Boolean = align(r1) == r2

    val foo = ("age" ->> 34) :: ("name" ->> "Jane") :: HNil
    val bar = ("name" ->> "Jane") :: ("age" ->> 34) :: HNil

    val result = permutatedEqual(foo, bar)

    assert(result === true)
  }

  test("field values are different, object are same if we are using align from shapeless") {
    def permutatedEqual[R1 <: HList, R2 <: HList](
                                                   r1: R1, r2: R2
                                                   )(implicit
                                                     align: Align[R1, R2]
                                                   ): Boolean = align(r1) == r2

    val foo = ("age" ->> 34) :: ("name" ->> "Jane") :: HNil
    val bar = ("name" ->> "Jane") :: ("age" ->> 30) :: HNil

    val result = permutatedEqual(foo, bar)

    assert(result === false)
  }

}
