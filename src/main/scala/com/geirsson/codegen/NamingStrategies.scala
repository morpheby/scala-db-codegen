package com.geirsson.codegen

import caseapp.core.ArgParser
import io.getquill.NamingStrategy

object SnakeCaseReverse extends SnakeCaseReverse

trait SnakeCaseReverse extends NamingStrategy {
  override def column(s: String): String = {
    val camelCased = default(s)
    camelCased.head.toLower + camelCased.tail
  }

  override def default(s: String): String = {
    s.toLowerCase
      .split("_")
      .map { // avoid possible collisions caused by multiple '_'
        case "" => "_"
        case s => s
      }
      .map(_.capitalize)
      .mkString("")
  }
}

object EscapingSnakeCaseReverse extends NamingStrategy {
  override def column(s: String): String = {
    val camelCased = default(s)
    val lowered = camelCased.head.toLower + camelCased.tail
    if (ReservedNames.isReserved(lowered))
      s"`$lowered`"
    else
      lowered
  }

  override def default(s: String): String = {
    s.toLowerCase
      .split("_")
      .map { // avoid possible collisions caused by multiple '_'
        case "" => "_"
        case s => s
      }
      .map(_.capitalize)
      .mkString("")
  }
}


trait SupportedNamingStrategies {
}

object SupportedNamingStrategies {
  implicit val namingStrategyConvert: ArgParser[NamingStrategy with SupportedNamingStrategies] =
    ArgParser.instance("naming strategy") { s =>
      NamingStrategyMap(s)(lift).map(Right(_))
        .getOrElse(Left("Invalid naming strategy. Available strategies: " + NamingStrategyMap.strategies.mkString(", ")))
    }

  private object NamingStrategyMap {
    private val strategyMap = Map(
      "SnakeCaseReverse" -> SnakeCaseReverse,
      "EscapingSnakeCaseReverse" -> EscapingSnakeCaseReverse,
    )

    def strategies = strategyMap.keys
    def apply[T](name: String)(implicit conv: NamingStrategy => T): Option[T] =
      strategyMap.get(name).map(x => x)
  }

  implicit def lift(namingStrategy: NamingStrategy): NamingStrategy with SupportedNamingStrategies = {
    new NamingStrategy with SupportedNamingStrategies {
      override def default(s: String): String = namingStrategy.default(s)
      override def column(s: String): String = namingStrategy.column(s)
      override def table(s: String): String = namingStrategy.table(s)
    }
  }
}
