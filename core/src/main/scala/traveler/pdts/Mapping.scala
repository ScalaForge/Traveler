package traveler.pdts

import traveler.Target
import traveler.pdts.NumericPDT.NumericTypes
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import traveler.Target.LinuxX64

trait Mapping:
  type M[_ <: Target] <: Matchable
  def apply(t: Target): Unit = ???

trait NumericMapping:
  type M[T <: Target] <: NumericPDT.NumericTypes
  def apply(t: Target): Int

object NumericMapping:
  transparent inline def create[_M[_ <: Target] <: PDTNumeric.NumericTypes]
      : NumericMapping =
    ${ _create[_M] }

  private def _orToList(using q: Quotes)(
      typ: q.reflect.TypeRepr
  ): List[q.reflect.TypeRepr] =
    import scala.quoted.quotes.reflect.*
    typ match
      case OrType(a, b) =>
        _orToList(a) ++ _orToList(b)
      case AndType(_, _) => report.errorAndAbort("No ands")
      case t             => List(t)

  private def getMatchType(using q: Quotes)(
      typ: q.reflect.TypeRepr
  ): Option[q.reflect.TypeRepr] =
    import scala.quoted.quotes.reflect.*
    typ match
      case t @ TypeRef(prefix, name) =>
        println("here")
        val x = TypeRepr.of[traveler.M]

        // TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "traveler"), "Main$package"), "M")
        val s = "traveler.M"
        val symbol = Symbol.requiredClass(s).typeRef

        getMatchType(t.translucentSuperType)

      // report.errorAndAbort(s"${x.show(using Printer.TypeReprStructure)}")
      // report.errorAndAbort(s"${x =:= symbol}")
      // None
      // getMatchType(prefix.memberType(Symbol.classSymbol(name)))
      case TypeLambda(_, _, r) => getMatchType(r)
      case m: MatchType        => Some(m)
      case NoPrefix            => None
      case _                   => None

  private def _create[_M[_ <: Target] <: PDTNumeric.NumericTypes](using
      Quotes,
      Type[_M]
  ): Expr[NumericMapping] =
    import scala.quoted.quotes.reflect.*

    // report.errorAndAbort(s"cannot find match type in ${TypeRepr.of[M]}")
    val patterns = getMatchType(TypeRepr.of[_M]).getOrElse(
      report.errorAndAbort(s"cannot find match type in ${TypeRepr.of[_M].show}")
    ) match
      case MatchType(_, _, cases) =>
        cases.map { case MatchCase(pattern, rhs) =>
          pattern -> rhs
        }

    val badPatterns = patterns
      .groupBy(_._2)
      .filter(_._2.size != 1)
      .values
      .flatten
      .map(p => s"${p._1.show} => ${p._2.show}")
    if badPatterns.nonEmpty then
      report.warning(
        s"[Error code 1] Multiple cases map to the same underlying type. Consider using a union type to merge these cases: ${badPatterns
            .mkString("\n\t", "\n\t", "")}"
      )
    // report.info(patterns.map(p => s"${p._1.show} -> ${p._2.show}").toString())

    // // report.errorAndAbort("hello")
    // report.errorAndAbort(TypeRepr.of[[A <: Target] =>> A].show(using Printer.TypeReprStructure))

    // def matchCode(target: Expr[Target])(using Quotes): Expr[Int] =
    //   // val term = target.asTerm
    //   // val res = Match(term, List(CaseDef(Literal(IntConstant(LinuxX64.id)), None, Literal(IntConstant(1))))).asExprOf[Int]
    //   // report.info(
    //   //   res.show
    //   // )

    //   '{${target}.id}

    // val matchCase =
    //   patterns.flatMap(p => _orToList(p._1).map(_ -> p._2))
    '{
      new NumericMapping:
        type M[T <: Target] = _M[T]
        def apply(t: Target): Int = 1
    }
