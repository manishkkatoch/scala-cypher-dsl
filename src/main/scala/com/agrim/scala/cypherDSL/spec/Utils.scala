package com.agrim.scala.cypherDSL.spec

private object Utils {
  def matchPropertyPattern(identifier: String, propertyName: String) = s"$propertyName: {${identifier}_$propertyName}"
}
