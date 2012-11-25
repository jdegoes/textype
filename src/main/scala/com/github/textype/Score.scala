package com.github.textype

import math._

case class ScoreColumnStats(min: Int, max: Int, mean: Double, stdDev: Double)

/**
 * A score for a particular format. Retains information on how much was parsed,
 * how many values were extracted, and optionally, column statistics.
 */
case class Score(parsed: Double, values: Int, columnStats: Option[ScoreColumnStats]) extends Ordered[Score] {
  def compare(that: Score): Int = {
    import math.{signum, round}

    round(signum((this.value - that.value).toFloat))
  }

  def value: Double = {
    // TODO: Pluggable scoring model
    columnStats.map { columnStats =>
      pow(columnStats.stdDev + 1.0, 2) / (values * parsed)
    }.getOrElse(Double.MaxValue)
  }
}