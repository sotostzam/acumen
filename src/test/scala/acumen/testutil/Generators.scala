package acumen
package testutil

import org.scalacheck._
import Gen._
import Shrink._
import Arbitrary.arbitrary

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
        for (o     <- resize(s/4, arbitrary[Expr]);
             field <- arbitrary[Name])
        yield Dot(o, field))
    )

  implicit def arbCreate : Arbitrary[Create] =
    Arbitrary (
      Gen.sized(s =>
        for (oe <- arbitrary[Option[Expr]];
             c <- arbitrary[ClassName];
             as <- listOf(resize(s/4, arbitrary[Expr])))
          yield Create(oe, c, as))
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
    
}
