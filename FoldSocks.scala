// solving Riddler Classic @ https://fivethirtyeight.com/features/can-you-fold-all-your-socks/

object FoldSocks extends App {

  // implementing rational numbers
  case class Fraction(n_ : Long, d_ : Long) {
    require(d_ != 0L)
    private val (_n: Long, _d: Long) = if (n_ == 0L) (0L, 1L) else {
      val sign = if (n_ == 0L || n_ > 0L && d_ > 0L || n_ < 0L && d_ < 0L) 1L else -1L
      val n_abs = Math.abs(n_)
      val d_abs = Math.abs(d_)
      val this_gcd = Fraction._gcd(n_abs, d_abs)
      (sign * n_abs / this_gcd, d_abs / this_gcd)
    }
    lazy val toDouble: Double = _n.toDouble / _d
    def n: Long = _n
    def d: Long = _d
    def +(that: Fraction): Fraction = Fraction(n * that.d + that.n * d, d * that.d)
    def -(that: Fraction): Fraction = Fraction(n * that.d - that.n * d, d * that.d)
    def *(that: Fraction): Fraction = Fraction(n * that.n, d * that.d)
    def /(that: Fraction): Fraction = Fraction(n * that.d, d * that.n)
    override def toString = s"${_n}" + (if (_d != 1L) s" / ${_d} ~= ${toDouble}" else "")
  }

  object Fraction {
    def apply(i: Long): Fraction = Fraction(i, 1)

    def gcd(a: Long, b: Long): Long = {
      require(a > 0 && b > 0)
      _gcd(a, b)
    }

    private def _gcd(a: Long, b: Long): Long = {
      def gcdOfSortedArguments(larger: Long, smaller: Long): Long = {
        val remainder = larger % smaller
        if (remainder == 0L) smaller
        else gcdOfSortedArguments(smaller, remainder)
      }
      if (a >= b) gcdOfSortedArguments(a, b)
      else gcdOfSortedArguments(b, a)
    }
  }

  // problem input
  case class Input(nrSocksFittingOnChair: Int, nrRemainingSockPairs: Int, nrSocksExtracted: Int) {
    lazy val nrSocksYetToBeExtracted: Int = 2 * nrRemainingSockPairs - nrSocksExtracted
    lazy val probabilityThatNextExtractedSockIsMatch: Fraction = Fraction(nrSocksExtracted, nrSocksYetToBeExtracted)
    lazy val probabilityThatNextExtractedSockIsNoMatch: Fraction = Fraction(1) - probabilityThatNextExtractedSockIsMatch

    lazy val afterMatch: Input = copy(nrRemainingSockPairs = nrRemainingSockPairs - 1, nrSocksExtracted = nrSocksExtracted - 1)

    lazy val afterNoMatch: Input = copy(nrSocksExtracted = nrSocksExtracted + 1)

    override def toString: String = s"C=$nrSocksFittingOnChair, r=$nrRemainingSockPairs, e=$nrSocksExtracted"
  }

  // compute the "success probability" that all sock pairs can be matched without ever running out of places on the chair
  def successProbability(input: Input, doPrint: Boolean): Fraction = {
    val calculatedSuccessProbabilities = collection.mutable.Map.empty[Input, Fraction]

    def successProbabilityMemoized(input: Input, doPrint: Boolean): Fraction = {
      if (input.nrRemainingSockPairs <= input.nrSocksFittingOnChair) Fraction(1)
      else if (input.nrSocksExtracted > input.nrSocksFittingOnChair || input.nrSocksExtracted < 0) Fraction(0)
      else calculatedSuccessProbabilities.getOrElse(input, {
        val probability =
          input.probabilityThatNextExtractedSockIsMatch * successProbabilityMemoized(input.afterMatch, doPrint) +
            input.probabilityThatNextExtractedSockIsNoMatch * successProbabilityMemoized(input.afterNoMatch, doPrint)
        calculatedSuccessProbabilities(input) = probability
        println(s"s(${input}) = $probability")
        probability
      })
    }

    successProbabilityMemoized(input, doPrint)
  }

  successProbability(Input(9, 14, 0), doPrint = true)
}
