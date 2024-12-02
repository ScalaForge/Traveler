package traveler.pdts

import scala.quoted.Quotes
import scala.quoted.Expr

object Macro {
  def mac(using Quotes): Expr[Int] =
    import quoted.quotes.reflect.*

    report.errorAndAbort(
      TypeRepr.of[PDT].dealiasKeepOpaques.typeSymbol.exists.toString()
    )
    '{ 5 }

  inline def myMac: Int = ${
    mac
  }
}
