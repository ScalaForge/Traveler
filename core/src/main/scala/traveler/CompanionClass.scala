package traveler

import scala.quoted.*

trait CompanionClass[A]:
  type Type

object CompanionClass:
  transparent inline given [A]: CompanionClass[A] = ${ companionImpl[A] }
  def companionImpl[A: Type](using Quotes) =
    import quotes.reflect.*
    val companionClass = TypeRepr.of[A].typeSymbol.companionModule.typeRef
    if companionClass.typeSymbol.isNoSymbol then report.errorAndAbort(s"No companion class found for ${Type.show[A]}")
    else companionClass.asType match
      case '[t] => 
        '{
          new CompanionClass[A]:
            type Type = t
        }
      
    
