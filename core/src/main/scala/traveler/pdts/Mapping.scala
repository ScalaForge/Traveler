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
  transparent inline def create[M[_ <: Target] <: PDTNumeric.NumericTypes]: NumericMapping = 
    ${_create[M]}


  private def _orToList(using q: Quotes)(typ: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = 
    import scala.quoted.quotes.reflect.*
    typ match
      case OrType(a,b) => 
        _orToList(a) ++ _orToList(b)
      case AndType(_,_) => report.errorAndAbort("No ands")
      case t => List(t)

  private def getMatchType(using q: Quotes)(typ: q.reflect.TypeRepr): q.reflect.TypeRepr = 
    import scala.quoted.quotes.reflect.*
    typ match 
      case TypeRef(ref, _) => getMatchType(ref)
      case TypeLambda(_, _, r) => getMatchType(r)
      case m: MatchType => m
    
  private def _create[M[_ <: Target] <: PDTNumeric.NumericTypes](using Quotes, Type[M]): Expr[NumericMapping] = 
    import scala.quoted.quotes.reflect.* 


    val patterns = getMatchType(TypeRepr.of[M]) match 
      case MatchType(_, _, cases) => cases.flatMap{
        case MatchCase(pattern, rhs) => _orToList(pattern)
      }

    report.errorAndAbort(patterns.map(_.show).toString())

    ???