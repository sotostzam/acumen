package acumen
package util


import Names._
import Canonical._
import Conversions._

import scala.collection.mutable.HashMap  
import scala.collection.mutable.ArrayBuffer
import scala.Stream

object Filters {
  
  type Summary3D = 
    HashMap[
      CId,
      Tuple4[Int,ArrayBuffer[Double],ArrayBuffer[Double],ArrayBuffer[Double]]]
  type Summary2D = 
    HashMap[CId,Tuple3[Int,ArrayBuffer[Double],ArrayBuffer[Double]]]
  
  def onlyAfterContinuous(h:Stream[CStore]) = {
    def helper(flag:Boolean, h:Stream[CStore]) : Stream[CStore] = {
      if (h.isEmpty) Stream.empty
      else if (getResultType(h.head) == FixedPoint) helper(true, h.tail)
      else { if (flag) h.head #:: helper(false, h.tail) else helper(false, h.tail) } 
    }
    helper(false, h)
  }

  def addThirdDimension(h:Stream[CStore]) : Stream[CStore] = {
    val ny = name("y")
    val nz = name("z")
    h map {
      s => {
        s transform {
          case (_,o) => {
            if (o contains ny) {
              val y = o(ny)
              o.updated(ny,VLit[CId](GDouble(0))).updated(nz,y)
            } else o
          } 
        }
      }
    }
  }
  
  def toSummary2D(h:Stream[CStore]) : Summary2D = {
    val nx = name("x")
    val ny = name("y")
    def get(e:CObject,n:Name) = { val VLit(GDouble(x)) = e(n); x }
    
    var res = new HashMap[CId,Tuple3[Int,ArrayBuffer[Double],ArrayBuffer[Double]]]()
    var counter = 0
    for (s <- h) {
      for ((i,o) <- s; if o contains nx; if o contains ny) {
        try {
          val x = get(o,nx);
          val y = get(o,ny)
          if (res contains i) {
            val (s,xs,ys) = res(i); 
            xs += x; ys += y;  
          } else {
            val xs = new ArrayBuffer[Double]()
            val ys = new ArrayBuffer[Double]()
            xs += x; ys += y; 
            res(i) = (counter, xs, ys)
          }
        } catch { case _ => } 
      }
      counter = counter + 1
    }
    res
  }
  
  def toSummary3D(h:Stream[CStore]) : Summary3D = {
    val nx = name("x")
    val ny = name("y")
    val nz = name("z")
    def get(e:CObject, n:Name) = { val VLit(GDouble(x)) = e(n); x }
    
    var res = new HashMap[
      CId,
      Tuple4[Int,ArrayBuffer[Double],ArrayBuffer[Double],ArrayBuffer[Double]]]()
    var counter = 0
    for (s <- h) {
      for ((i,o) <- s; if o contains nx; if o contains ny; if o contains nz) {
        try {
          val x = get(o,nx);
          val y = get(o,ny)
          val z = get(o,nz)
          if (res contains i) {
            val (s,xs,ys,zs) = res(i); 
            xs += x; ys += y; zs += z;  
          } else {
            val xs = new ArrayBuffer[Double]()
            val ys = new ArrayBuffer[Double]()
            val zs = new ArrayBuffer[Double]()
            xs += x; ys += y; zs += z;
            res(i) = (counter, xs, ys, zs)
          }
        } catch { case _ => } 
      }
      counter = counter + 1
    }
    res
  }

}
