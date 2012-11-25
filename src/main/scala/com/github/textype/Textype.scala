package com.github.textype

import detectors._

/**
 * Bundles all the built-in detectors into a single detector.
 *
 * {{{
 * import Textype._
 * val format = detect(sample).head
 * }}}
 */
trait Textype extends Detector {
  private val detectors: Seq[Detector] = CSVDetector :: Nil

  private val detector = detectors.foldLeft(Detector.Zero)(_ |+| _)

  def apply(value: String) = detector.apply(value)
}
object Textype extends Textype