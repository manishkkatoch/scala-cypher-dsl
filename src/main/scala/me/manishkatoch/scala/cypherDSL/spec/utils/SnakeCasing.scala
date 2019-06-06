package me.manishkatoch.scala.cypherDSL.spec.utils

private[spec] trait SnakeCasing {
  private def toUpperSnakeCase(label: String): String = {
    label
      .flatMap(char => {
        if (char.isUpper) Seq("_", char)
        else Seq(char)
      })
      .mkString
      .stripPrefix("_")
      .toUpperCase
  }

  def upperSnakeCased(string: String): String = toUpperSnakeCase(string)
}
