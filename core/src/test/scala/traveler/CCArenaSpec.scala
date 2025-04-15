package traveler

class CCArenaSpec extends munit.FunSuite:
  test("shared arena catches leaks"):
    assertNoDiff(
      compileErrors {
        """CCArena.shared{(a: CCSharedArena^) ?=>
          a.get.toString()
    }
    """
      },
      ""
    )

  test("multiple shared arenas are allowed"):
    CCArena.shared:
      val a = CCArena()
      val r = CCArena.shared:
        a.get
      r.toString()

  test("multiple confined arenas are allowed"):
    CCArena.confined:
      val a = CCArena()
      val r = CCArena.confined:
        a.get
      r.toString()

  test("shouldn't compile"):
    CCArena.confined:
      val a = CCArena()
      val b: CCSharedArena^ = ???
      val r = NThread.inner(using b):
        b.get
      r.toString()
