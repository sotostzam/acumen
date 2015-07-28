package acumen.util

/* port of (part of) the haskell Random package */

object Random {
  type Gen = (Int, Int)

  def mkGen(s: Int): Gen = {
    if (s < 0) mkGen(-s)
    else {
      val (q, s1) = {
        val n = 2147483562
        (s / n, s % n)
      }
      val s2 = q % 2147483398
      (s1 + 1, s2 + 1)
    }
  }

  def next(g: Gen): (Int, Gen) = {
    val (s1, s2) = g

    lazy val zp = if (z < 1) z + 2147483562 else z
    lazy val z = s1pp - s2pp

    lazy val k = s1 / 53668
    lazy val s1p = 40014 * (s1 - k * 53668) - k * 12211
    lazy val s1pp = if (s1p < 0) s1p + 2147483563 else s1p

    lazy val kp = s2 / 52774
    lazy val s2p = 40692 * (s2 - kp * 52774) - kp * 3791
    lazy val s2pp = if (s2p < 0) s2p + 2147483399 else s2p

    (zp, (s1pp, s2pp))
  }

  def split(g: Gen): (Gen, Gen) = {
    val (s1, s2) = g
    lazy val left = (new_s1, t2)
		lazy val right = (t1, new_s2)
    lazy val new_s1 = if (s1 == 2147483562) 1 else s1 + 1
    lazy val new_s2 = if (s2 == 1) 2147483398 else s2 - 1
    lazy val (t1, t2) = next(g)._2
    (left, right)
  }

	private def iLogBase(b:Int, i:Int) : Int =
		if (i < b) 1 else 1 + iLogBase(b, i / b)


	def randomIvalInt (low:Int, high:Int, rng:Gen) : (Int, Gen) = {
		if (low > high) 
			randomIvalInt(high, low, rng)
		else {
			lazy val k = high - low  + 1
			lazy val b = 2147483561
			lazy val n = iLogBase(b,k)

			def f(i:Int, acc:Int, g:Gen) : (Int, Gen) = {
				if (i == 0) 
					(acc, g)
				else {
					val (x,gp) = next(g)
					f(i-1, x+acc*b, gp)
				}
			}
			
			lazy val (v, rngp) = f(n,1,rng)
			(low + (v % k), rngp)
		}
	}

	private val intRange: Long = Int.MaxValue - Int.MinValue

  def randomIvalDouble (l:Double, h:Double, rng:Gen) : (Double, Gen) = {
		if (l>h) randomIvalDouble(h,l,rng)
		else {
			val (x,rngp) = randomIvalInt(Int.MinValue, Int.MaxValue, rng)
			val scaled_x = ((l+h)/2) + ((h-l) / intRange) * x
			(scaled_x, rngp)
		}

	}

	def randomBool(g:Gen) : (Boolean, Gen) = {
		val (i,gp) = randomIvalInt(0, 1, g)
		(if (i==0) false else true, gp)
	}

}

