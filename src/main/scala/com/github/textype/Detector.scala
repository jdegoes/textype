package com.github.textype

/**
 * Detects the format of a given sample, providing a stream of possibilities 
 * in decreasing order of probability.
 */
trait Detector extends (String => Stream[TextFileType]) { self =>
  /**
   * Combines two detectors into a single detector.
   */
  def |+| (that: Detector) = Detector((value: String) => {
    def merge(s1: Stream[TextFileType], s2: Stream[TextFileType]): Stream[TextFileType] = {
      (s1.headOption, s2.headOption) match {
        case (None, None) => Stream.empty
        case (Some(h), None) => s1
        case (None, Some(h)) => s2
        case (Some(h1), Some(h2)) =>
          if (h1.score.value <= h2.score.value) Stream.cons(h1, merge(s1.tail, s2))
          else Stream.cons(h2, merge(s1, s2.tail))
      }
    }

    merge(self(value), that(value))
  })
}

object Detector extends ((String => Stream[TextFileType]) => Detector) {
  final def apply(f: String => Stream[TextFileType]): Detector = new Detector {
    def apply(value: String): Stream[TextFileType] = f(value)
  }

  val Zero = Detector(value => Stream.empty[TextFileType])
}