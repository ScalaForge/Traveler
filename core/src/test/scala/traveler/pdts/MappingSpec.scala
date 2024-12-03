package traveler.pdts

import traveler.Target
import traveler.pdts.PDT
import traveler.Target.LinuxX64

class MappingSpec extends munit.FunSuite:
  type M[T <: Target] = T match
    case Target.LinuxX64 => Short
    case Target.MacX64   => Int
    case Target.WinX64   => Long

  opaque type TestType <: PDT = PDT
  object TestType:
    given Mapping[TestType, M] = Mapping.derive
  test("PDT should instantiate with short on Linux"):
    given LinuxX64 = Target.test[LinuxX64]
    assertEquals(
      compileErrors(
        """import TestType.given
           given Target.LinuxX64 = Target.test
           PDT[TestType](5: Short)}"""
      ),
      ""
    )

  test("PDT should instatiate with int on Mac"):
    assertEquals(
      compileErrors(
        """import TestType.given
           given Target.MacX64 = Target.test
           PDT[TestType](5)"""
      ),
      ""
    )

  test("PDT should instantiate with long on Windows"):
    assertEquals(
      compileErrors(
        """import TestType.given
           given Target.WinX64 = Target.test
           PDT[TestType](5l)"""
      ),
      ""
    )

  test("PDT should instantiate appropriately with unspecific"):
    {
      import TestType.given
      given Target = Target.test[LinuxX64]
      assertEquals(PDT[TestType].unspecific(5L), None)
      assertEquals(PDT[TestType].unspecific(15), None, "Int test")
      assert(PDT[TestType].unspecific(5: Short).isDefined)
    }
