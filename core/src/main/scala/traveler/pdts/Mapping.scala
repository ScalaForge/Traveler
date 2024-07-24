package traveler.pdts

import traveler.Target
import traveler.pdts.NumericPDT.NumericTypes
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type


trait Mapping:
  type M[_ <: Target] <: Matchable
  def apply(t: Target): Unit = ???

trait NumericMapping:
  type M[_ <: Target] <: NumericTypes
  def apply(t: Target): Int

object NumericMapping:
  transparent inline def create[M[_ <: Target] <: PDTNumeric.NumericTypes]
      : NumericMapping =
    ${ _create[M] }

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
      case t @ TypeRef(prefix, name)         => println("here")
        val x = TypeRepr.of[traveler.M]

        //TypeRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "<root>")), "traveler"), "Main$package"), "M")
        val s = "traveler.M"
        val symbol = Symbol.requiredClass(s).typeRef
        

        getMatchType(t.translucentSuperType)


        //report.errorAndAbort(s"${x.show(using Printer.TypeReprStructure)}")
        //report.errorAndAbort(s"${x =:= symbol}")
        //None
        //getMatchType(prefix.memberType(Symbol.classSymbol(name)))
      case TypeLambda(_, _, r) => getMatchType(r)
      case m: MatchType        => Some(m)
      case NoPrefix            => None
      case _                   => None

  private def _create[M[_ <: Target] <: PDTNumeric.NumericTypes](using
      Quotes,
      Type[M]
  ): Expr[NumericMapping] =
    import scala.quoted.quotes.reflect.*

    // report.errorAndAbort(s"cannot find match type in ${TypeRepr.of[M]}")
    val patterns = getMatchType(TypeRepr.of[M]).getOrElse(
      report.errorAndAbort(s"cannot find match type in ${TypeRepr.of[M].show}")
    ) match
      case MatchType(_, _, cases) =>
        cases.flatMap { case MatchCase(pattern, rhs) =>
          _orToList(pattern)
        }

    // // report.errorAndAbort("hello")
    //report.errorAndAbort(TypeRepr.of[[A <: Target] =>> A].show(using Printer.TypeReprStructure))

    
    val tl = TypeLambda(List("T"), _ => List(TypeBounds.lower(TypeRepr.of[Target])), tl => TypeRepr.of[M].appliedTo(tl.typeArgs))
    
    //MatchType(TypeRepr.of[NumericPDT.NumericTypes], )

    tl.asType match 
      case '[[T] =>> Option] => 
        '{
          new NumericMapping:
            type M[T] = [T] =>> Option
            def apply(t: Target): Int = 5
        }
      case _ => report.errorAndAbort("uhoh")

