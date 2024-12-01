package traveler.pdts

import traveler.Target
import traveler.pdts.NumericPDT.NumericTypes
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import traveler.Target.LinuxX64
import traveler.Target.MacX64
import traveler.Target.WinX64

trait Mapping:
  type M[_ <: Target] <: Matchable
  def apply(t: Target): Unit = ???

type NumericTypes = Byte | Short | Int | Long | Float | Double
trait NumericMapping[M[_ <: Target] <: NumericTypes]:
  val fn: Target => Int 
  def apply(t: Target): Int = fn(t)
  type _M[T <: Target] = NumericTypes

object NumericMapping:
  transparent inline def create[_M[_ <: Target] <: NumericTypes]
      : NumericMapping[?] =
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

  private def _create[_M[_ <: Target] <: NumericTypes](using
      Quotes,
      Type[_M]
  ): Expr[NumericMapping[?]] =
    import scala.quoted.quotes.reflect.*

    // def matchCode(target: Expr[Target])(using Quotes): Expr[Int] =
    //   val term = '{$target.id}.asTerm
    //   val res = Match(term, List(CaseDef(Literal(IntConstant(LinuxX64.id)), None, Literal(IntConstant(1))))).asExprOf[Int]

    //   res


    val tr = TypeRepr.of[_M] .asType


    val tt = '{
      type M[T <: Target] <: NumericTypes = T match 
        case Target.LinuxX64 | Target.MacX64 => Int
        case Target.WinX64 => Float
    }.asTerm
    ???

