package acumen
package testutil

import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary
import acumen.interpreters.enclosure.Interval
import acumen.interpreters.enclosure.Generators.{
  genSubInterval
}
import acumen.interpreters.enclosure.TestingContext.{
  rnd
}

/* scalacheck generators */

object Generators  {

  /* 
     -TODO: finish shrinking for even smaller counter examples
     -TODO: add missing cases to arbitrary (can we statically check that we
            don't miss a constructor?)
  */

  def myPosNum = resize(Integer.MAX_VALUE, posNum[Int])

  def legit(x:String) = 
    x.length > 0 && x.head.isLetter && !(Parser.lexical.reserved contains x) 

  implicit def arbName : Arbitrary[Name] = 
  	Arbitrary {
      for { x <- alphaChar
            xs <- resize(10, listOf1(alphaNumChar)).map(x + _.mkString) suchThat legit
            i <- oneOf(List(0,1,2,3))
      } yield Name(xs, i)
  	}

  implicit def shrinkName : Shrink[Name] =
    Shrink { n =>
      n match {
        case Name(xs,i) => 
          for ((nxs,ni) <- shrink((xs,i)) if legit(nxs))
            yield Name(nxs,ni)
      }
    }
  
  implicit def arbClassName : Arbitrary[ClassName] = 
  	Arbitrary {
      for { x <- alphaChar
            xs <- resize(10, listOf1(alphaNumChar)).map(x + _.mkString) suchThat legit
      } yield ClassName(xs)
  	}

  implicit def shrinkClassName : Shrink[ClassName] =
    Shrink { n =>
      n match {
        case ClassName(xs) => 
          for (nxs <- shrink(xs) if legit(nxs))
            yield ClassName(nxs)
      }
    }

  implicit def arbProg : Arbitrary[Prog] =
    Arbitrary (arbitrary[List[ClassDef]].map(Prog))

  implicit def shrinkProg : Shrink[Prog] =
    Shrink { p =>
      p match {
        case Prog(ds) => 
          for (nds <- shrink(ds))
            yield Prog(nds)
      }
    }

  implicit def arbClassDef : Arbitrary[ClassDef] =
    Arbitrary {
      for(c  <- arbitrary[ClassName];
          fs <- arbitrary[List[Name]];
          b  <- arbitrary[List[Action]])
        yield ClassDef(c, fs, List(), b)
    }

  implicit def shrinkClassDef : Shrink[ClassDef] =
    Shrink { cd =>
      cd match {
        case ClassDef(c,fs,pvs,b) => 
          for ((nc,nfs,npvs,nb) <- shrink((c,fs,pvs,b)))
            yield ClassDef(nc,nfs,npvs,nb)
      }
    }

  implicit def arbAction : Arbitrary[Action] = 
    Arbitrary ( arbitrary[IfThenElse] | arbitrary[ForEach] | arbitrary[Switch]
              | arbitrary[Discretely] | arbitrary[Continuously] )

  implicit def shrinkAction : Shrink[Action] =
    Shrink { a =>
      a match {
        case IfThenElse(c,t,e) => 
          for ((nc,nt,ne) <- shrink((c,t,e)))
            yield IfThenElse(nc,nt,ne)
        case ForEach(i,c,b) =>
          for((ni,nc,nb) <- shrink((i,c,b)))
            yield ForEach(ni,nc,nb)
        case Continuously(ca) => shrink(ca) map Continuously
        case Discretely(da) => shrink(da) map Discretely
        case Switch(s,cls) =>
          for((ns,ncls) <- shrink((s,cls)))
            yield Switch(ns,ncls)
      }
    }

  implicit def arbDiscretely : Arbitrary[Discretely] =
    Arbitrary ( arbitrary[DiscreteAction] map Discretely )
  
  implicit def arbContinuously : Arbitrary[Continuously] =
    Arbitrary ( arbitrary[ContinuousAction] map Continuously )

  implicit def arbDiscreteAction : Arbitrary[DiscreteAction] =
    Arbitrary ( arbitrary[Assign] | arbitrary[Create] | arbitrary[Move] )

  implicit def arbContinuousAction : Arbitrary[ContinuousAction] =
    Arbitrary ( arbitrary[Equation] )

  implicit def arbEquation : Arbitrary[Equation] =
    Arbitrary {
      for (lhs <- arbitrary[Expr];
           rhs <- arbitrary[Expr])
        yield Equation(lhs, rhs)
    }

  implicit def shrinkEquation : Shrink[Equation] =
    Shrink { e =>
      e match {
        case Equation(l,r) => 
          for ((nl,nr) <- shrink((l,r)))
            yield Equation(nl,nr)
      }
    }

  implicit def arbAssign : Arbitrary[Assign] =
    Arbitrary {
      for (lhs <- arbitrary[Expr];
           rhs <- arbitrary[Expr])
        yield Assign(lhs, rhs)
    }

  implicit def shrinkAssign : Shrink[Assign] =
    Shrink { e =>
      e match {
        case Assign(l,r) => 
          for ((nl,nr) <- shrink((l,r)))
            yield Assign(nl,nr)
      }
    }

  implicit def arbIfThenElse : Arbitrary[IfThenElse] =
    Arbitrary {
      Gen.sized(s =>
        for (c <- resize(s/4, arbitrary[Expr]);
             t <- resize(s/4, arbitrary[List[Action]]);
             e <- resize(s/4, arbitrary[List[Action]]))
          yield IfThenElse(c, t, e))
    }

  implicit def arbSwitch : Arbitrary[Switch] =
    Arbitrary {
      Gen.sized(s =>
        for (c <- resize(s/4, arbitrary[Expr]);
             b <- resize(s/4, arbitrary[List[Clause]]))
          yield Switch(c, b))
    }

  implicit def arbClause : Arbitrary[Clause] = 
    Arbitrary {
      for {
        lhs <- arbitrary[GroundValue]
        inv <- arbitrary[Expr]
        rhs <- arbitrary[List[Action]]
      } yield Clause(lhs, inv, rhs)
    }

  implicit def shrinkClause : Shrink[Clause] =
    Shrink { e =>
      e match {
        case Clause(l, i, r) =>
          for ((nl, ni, nr) <- shrink((l, i, r)))
            yield Clause(nl, ni, nr)
      }
    }

  implicit def arbForEach : Arbitrary[ForEach] =
    Arbitrary {
      Gen.sized(s =>
        for (i <- arbitrary[Name];
             c <- resize(s/4, arbitrary[Expr]);
             a <- resize(s/4, arbitrary[List[Action]]))
          yield ForEach(i, c, a))
    }

  implicit def arbExpr : Arbitrary[Expr] =
    Arbitrary ( arbitrary[GroundValue].map(Lit) | arbitrary[Var] | arbitrary[Op] 
              | arbitrary[Dot] )
  
  implicit def arbLit : Arbitrary[GroundValue] =
    Arbitrary ((arbitrary[GInt] | arbitrary[GDouble] 
               | arbitrary[GBool] | arbitrary[GStr]))
							 
  
  implicit def arbBool : Arbitrary[GBool] = 
    Arbitrary (arbitrary[Boolean].map(GBool))
  
  implicit def arbStr : Arbitrary[GStr] = 
    Arbitrary (alphaStr.map(GStr))
  
  implicit def arbInt : Arbitrary[GInt] = 
    Arbitrary (myPosNum map GInt)
  
	/* New generators */
	/* Value class has an type of CId, see AST file for details */
	implicit def arbLitGroundValue : Arbitrary[VLit] =
		Arbitrary {
			arbitrary[GroundValue].map(VLit)
		/*
			val genLitInt    :Gen[VLit[GroundValue]]   = for(e <- Arbitrary.arbitrary[GInt])    yield VLit(e)
			val genLitDouble :Gen[VLit[GroundValue]]   = for(e <- Arbitrary.arbitrary[GDouble]) yield VLit(e)
			val genLitStr    :Gen[VLit[GroundValue]]   = for(e <- Arbitrary.arbitrary[GStr])    yield VLit(e)
			val genLitBool   :Gen[VLit[GroundValue]]   = for(e <- Arbitrary.arbitrary[GBool])   yield VLit(e)
			Gen.frequency((2, genLitInt), (2, genLitDouble), (2, genLitStr), (2, genLitBool))
		*/
  }
	implicit def arbValue : Arbitrary[Value[CId]] = 
		Arbitrary(arbitrary[VLit] | arbitrary[VVector[CId]] )
	// TODO: Arbitrary generated VVector will casue over flow, when vector's elements can also be vector	
	implicit def arbVVector : Arbitrary[VVector[CId]] = 
		Arbitrary{
			arbitrary[List[VLit]].map(VVector[CId])
		}	

  implicit def arbCObject: Arbitrary[CObject] =
    Arbitrary {
      for (
        ns <- listOf(arbitrary[Name]);
        vs <- listOf(arbitrary[CValue])
      ) yield (ns zip vs).toMap
    }

  implicit def arbCStore: Arbitrary[CStore] =
    Arbitrary {
      for (
        ids <- listOf(arbitrary[CId]);
        objs <- listOf(arbitrary[CObject])
      ) yield (ids zip objs).toMap
    }

  implicit def arbCId: Arbitrary[CId] =
    Arbitrary {
      for (ints <- listOf(posNum[Int])) yield new CId(ints)
    }

	/* End of new generators */

  implicit def arbIDoubleLit : Arbitrary[GDouble] = 
    Arbitrary (arbitrary[Double].map(GDouble))
 
  implicit def arbVar : Arbitrary[Var] =
    Arbitrary (arbitrary[Name].map(Var))
  
  implicit def arbOp : Arbitrary[Op] =
    Arbitrary (
      Gen.sized(s =>
      	for (op   <- arbitrary[Name];
      	     args <- listOf(resize(s/4, arbitrary[Expr])))
          yield Op(op, args))
    )
  
  implicit def arbDot : Arbitrary[Dot] =
    Arbitrary (
      Gen.sized(s =>
        for (o     <- arbitrary[Name];
             field <- arbitrary[Name])
        yield Dot(Var(Name(o.x, 0)), field))
    )

  implicit def arbCreate : Arbitrary[Create] =
    Arbitrary (
      Gen.sized(s =>
        for (oe <- arbitrary[Option[Expr]];
             c <- arbitrary[ClassName];
             as <- listOf(resize(s/4, arbitrary[Expr])))
          yield Create(oe, Var(Name(c.x,0)), as))
    )


  implicit def arbMove : Arbitrary[Move] =
    Arbitrary (
      Gen.sized(s =>
        for (p <- arbitrary[Expr];
             e <- arbitrary[Expr])
          yield Move(p, e))
    )
    
  def genSmallDouble = chooseNum[Double](-1.0, 1.0)

  /* Generator combinators */
	
  /**
   * Generates a set of n elements using genElem, each time passing a new 
   * element of the input list to genElem.
   */
  def genSetBasedOn[P, E](params: Set[P], gen: P => Gen[E]): Gen[Set[E]] =
    if (params.isEmpty) Set[E]()
    else for {
      head <- gen(params.head)
      tail <- genSetBasedOn(params.tail, gen)
    } yield tail + head

  /** Generates a set of distinct E */
  def genDistinctSetOfN[E](size: Int, gen: => Gen[E]): Gen[Set[E]] =
    genCompliantSetOfN(size, gen, (candidate, e) => e.forall(!candidate.contains(_)))

  /** Generates a set of E, where the set complies with condition */
  def genCompliantSetOfN[E](size: Int, gen: => Gen[E], condition: (Set[E],Set[E]) => Boolean): Gen[Set[E]] =
    if (size == 0) Set[E]()
    else for {
      init <- listOfN(size, gen)
      distinct = init.toSet
      rest <- genCompliantSetOfN(size - distinct.size, gen, condition) suchThat (condition(distinct,_))
    } yield distinct union rest

  /** Generates a set of E, where the set complies with condition */
  def genCompliantBoundedSet[E](min: Int, max: Int, gen: => Gen[E], condition: (Set[E],Set[E]) => Boolean): Gen[Set[E]] =
    if (max == 0) Set[E]()
    else for {
      size <- chooseNum[Int](min, max)
      res  <- genCompliantSetOfN(size, gen, condition)
    } yield res
    
  /** Generates a set of E whose members are not members of banned. */
  def genBoundedSetOfNonMembers[E](min: Int, max: Int, banned: Set[E], gen: => Gen[E]): Gen[Set[E]] =
    genCompliantBoundedSet(min, max, gen, (candidate, extension) => 
      extension.forall(n => !candidate.contains(n) && !banned.contains(n)))

  /** Generates a list of size s if cond is true and otherwise an empty list. */
  def listOfNIf[E](cond: Boolean, s: Int, gen: => Gen[E]): Gen[List[E]] =
    boundedListOfIf(cond, 0, s, gen)
    
  /** Generates a list of size between min and max if cond is true and otherwise an empty list. */
  def boundedListOfIf[E](cond: Boolean, min: Int, max: Int, gen: => Gen[E]): Gen[List[E]] =
    if (!cond) value(List()) else boundedListOf(min, max, gen)
    
  /** Generates a list of size between min and max. */
  def boundedListOf[E](min: Int, max: Int, gen: Gen[E]): Gen[List[E]] = {
    require(min <= max, "min (" + min + ") must be less than or equal to max (" + max + ")")
    require(min >= 0, "list size (" + min + ") cannot be negative")
    for { 
      i <- chooseNum[Int](min, max)
      l <- if (i == 0) value(List[E]()) else listOfN(i, gen) 
    } yield l
  }

  /* Utilities */
    
  /**
   * Given a list of Names, completes the list by adding names with lower primes.
   * E.g. the input:
   *   List(Name(2,"x"), Name(0,"y")) 
   * yields
   *   List(Name(1,"x"), Name(1,"x"), Name(2,"x"), Name(0,"y"))  
   */
  def completeNames(l: Set[Name]): Set[Name] =
    l.flatMap(n => if(n.primes == 0) List(n) else for(p <- 0 to n.primes) yield Name(n.x, p))
  def completeDots(l: Set[Dot]): Set[Dot] =
    l.flatMap(n => if(n.field.primes == 0) List(n) else for (p <- 0 to n.field.primes) yield Dot(n.obj, Name(n.field.x, p)))
    
  /** Generates a subinterval of [0, max]. */
  def genPositiveBoundedInterval(max: Double): Gen[Interval] =
    for {
      m <- choose[Double](Double.MinPositiveValue, max)
      i <- genSubInterval(Interval(0, m))
    } yield i 
    
}
