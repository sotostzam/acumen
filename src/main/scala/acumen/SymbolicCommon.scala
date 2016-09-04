package acumen
import GE._
import Pretty._
import Errors._

// Symbolic common evaluations
object SymbolicCommon {

  def isMatrix(m: ExprVector): Boolean = m.l(0) match {
    case ExprVector(_) => true
    case _             => false
  }
  def toMatrix(u: ExprVector): List[List[Expr]] = {
    if (isMatrix(u))
      u.l.asInstanceOf[List[ExprVector]].map(_.l)
    else
      List(u.l)
  }
  def listToArray(u: ExprVector): Array[Expr] = {
    u.l.toArray
  }

  def arrayToList(u: Array[Expr]): List[Expr] = {
    u.toList
  }
  def matrixToList(m: MatrixE): ExprVector = {
    ExprVector(m.map(x => ExprVector(arrayToList(x))).toList)
  }

  def toMatrixArray(u: ExprVector): Array[Array[Expr]] = {
    u.l(0) match {
      case ExprVector(_) => u.l.map(x => listToArray(x.asInstanceOf[ExprVector])).toArray
    }
  }

  def matrixMultiply[A](u: ExprVector, v: ExprVector): Expr = {
    try {
      val ul = toMatrix(u)
      val vl = toMatrix(v)
      val aun = ul.length; val avn = vl.length;
      val aum = ul(0).length; val avm = vl(0).length;
      if (aum != avn)
        throw error("Matrice's size aren't suitable for multiplication")

      val result = for (row <- ul)
        yield for (col <- vl.transpose)
        yield (row zip col).map(x => mkOp("*", x._1, x._2)).reduceLeft((r, y) => mkOp("+", r, y))

      if (result.length > 1)
        ExprVector(result.map(x => ExprVector(x)))
      else if (result(0).length == 1)
        result(0)(0)
      else
        ExprVector(result(0))
    } catch {
      case _: Throwable => error("Can't perform symbolci matrix multiplation")
    }
  }
  def symUnaryVectorOp(op: String, u: ExprVector): Expr = op match {
    case "trans" =>
      if (isMatrix(u)) {
        val m = toMatrix(u)
        m(0) match {
          // Column matrix to row vector
          case _ :: Nil => ExprVector(m.map(x => x(0)))
          case _        => ExprVector(m.transpose.map(ExprVector(_)))
        }

      } else ExprVector(u.l.map(x => ExprVector(x :: Nil)))
    case "inv"  => matrixToList(inverse(toMatrixArray(u)))
    case "norm" => mkOp(op, u)
  }
  // Symbolic evaluate operations, like vector * vector,
  // Although the entries might be unknown, we could still perform computation symbolically
  def symBinVectorOp(op: String, u: ExprVector, v: ExprVector): Expr = op match {
    case "+" => if (isMatrix(u) && isMatrix(v))
      ExprVector((u.l zip v.l).map(x =>
        symBinVectorOp(op, x._1.asInstanceOf[ExprVector], x._2.asInstanceOf[ExprVector])))
    else if (u.l.length == v.l.length)
      ExprVector((u.l zip v.l).map(x => mkOp("+", x._1, x._2)))
    else
      throw LengthVectorVectorOpExpr(u,v)
    case "-" => if (isMatrix(u) && isMatrix(v))
      ExprVector((u.l zip v.l).map(x =>
        symBinVectorOp(op, x._1.asInstanceOf[ExprVector], x._2.asInstanceOf[ExprVector])))
    else if (u.l.length == v.l.length)
      ExprVector((u.l zip v.l).map(x => mkOp("-", x._1, x._2)))
    else
      throw LengthVectorVectorOpExpr(u,v)
    case ".*" => if (isMatrix(u) && isMatrix(v))
      ExprVector((u.l zip v.l).map(x =>
        symBinVectorOp(op, x._1.asInstanceOf[ExprVector], x._2.asInstanceOf[ExprVector])))
    else
      ExprVector((u.l zip v.l).map(x => mkOp(".*", x._1, x._2)))
    case "dot" => mkBinOp("+", (u.l zip v.l).map(x => mkOp("*", x._1, x._2)))
    case "*"   => matrixMultiply(u, v)

  }
  def symBinScalarVectorOp(op: String, x: Expr, u: ExprVector): Expr = op match {
    case "+" => symBinVectorScalarOp(op, u, x)
    case "*" => symBinVectorScalarOp(op, u, x)
    case _   => mkOp(op, x, u)
  }
  def symBinVectorScalarOp(op: String, u: ExprVector, x: Expr): Expr = op match {
    case "+" => if (isMatrix(u))
      ExprVector(u.l.map(y =>
        symBinVectorScalarOp(op, y.asInstanceOf[ExprVector], x)))
    else
      ExprVector(u.l map (d => mkOp("+", d, x)))
    case "*" => if (isMatrix(u))
      ExprVector(u.l.map(y =>
        symBinVectorScalarOp(op, y.asInstanceOf[ExprVector], x)))
    else
      ExprVector(u.l map (d => mkOp("*", d, x)))
    case "/" => if (isMatrix(u))
      ExprVector(u.l.map(y =>
        symBinVectorScalarOp(op, y.asInstanceOf[ExprVector], x)))
    else
      ExprVector(u.l map (d => mkOp("/", d, x)))
    case ".^" => if (isMatrix(u))
      ExprVector(u.l.map(y =>
        symBinVectorScalarOp(op, y.asInstanceOf[ExprVector], x)))
    else
      ExprVector(u.l map (d => mkOp(".^", d, x)))
    case _ => println(mkOp(op, u, x)); mkOp(op, u, x)
  }
  type MatrixE = Array[Array[Expr]]
  def inverse(matrix: MatrixE): MatrixE = {
    if (matrix.length == 2 && matrix(0).length == 2) {
      val a = matrix(0)(0); val b = matrix(0)(1); val c = matrix(1)(0); val d = matrix(1)(1);
      val det = mkOp("/", Lit(GInt(1)), mkOp("-", mkOp("*", a, d), mkOp("*", b, c)))
      Array(Array(mkOp("*", det, d), mkOp("*", det, mkOp("*", Lit(GInt(-1)), b))),
        Array(mkOp("*", det, mkOp("*", Lit(GInt(-1)), c)), mkOp("*", det, a)))
    } else
      matrixScaleOp("*", transpose(cofactor(matrix)), mkOp("/", Lit(GInt(1)), determinant(matrix)))
  }

  def matrixScaleOp[A](op: String, au: MatrixE, gv: Expr): MatrixE = {
    for (row <- au)
      yield op match {
      case "+"  => row map (mkOp("+", _, gv))
      case "*"  => row map (mkOp("*", _, gv))
      case "/"  => row map (mkOp("/", _, gv))
      case ".^" => row map (mkOp(".^", _, gv))
    }
  }
  def transpose[A](matrix: MatrixE): MatrixE = {
    matrix.transpose
  }

  def cofactor(matrix: MatrixE): MatrixE = {
    val mat: Array[Array[Expr]] = Array.ofDim(matrixRow(matrix), matrixCol(matrix))
    for (i <- 0 to matrixRow(matrix) - 1)
      for (j <- 0 to matrixCol(matrix) - 1)
        mat(i)(j) = mkBinOp("*", Lit(GInt(changeSign(i))) :: Lit(GInt(changeSign(j))) :: determinant(createSubMatrix(matrix, i, j)) :: Nil)
    mat
  }
  def matrixRow(matrix: MatrixE): Int = matrix.length
  def matrixCol(matrix: MatrixE): Int = matrix(0).length
  def changeSign(i: Int): Int = if (i % 2 == 0) 1 else -1

  def createSubMatrix(matrix: MatrixE, excluding_row: Int, excluding_col: Int): MatrixE = {
    val mat: MatrixE = Array.ofDim(matrixRow(matrix) - 1, matrixCol(matrix) - 1)
    var r = -1
    for (i <- 0 to matrixRow(matrix) - 1; if i != excluding_row) {
      r = r + 1
      var c = -1
      for (j <- 0 to matrixCol(matrix) - 1; if j != excluding_col) {
        c = c + 1
        mat(r)(c) = matrix(i)(j)
      }
    }
    mat
  }

  def determinant(matrix: MatrixE): Expr = {
    if (matrix.length != matrix(0).length)
      error("Can't perform derminant operation because the size of matrix")
    var sum = Lit(GDouble(0.0)).asInstanceOf[Expr]
    var s: Double = 1
    if (matrix.length == 1) { //bottom case of recursion. size 1 matrix determinant is itself.
      matrix(0)(0)
    } else {
      for (i <- 0 to matrix.length - 1) { //finds determinant using row-by-row expansion
        var smaller: Array[Array[Expr]] = Array.ofDim(matrix.length - 1, matrix.length - 1) //creates smaller matrix- values not in same row, column
        for (a <- 1 to matrix.length - 1) {
          for (b <- 0 to matrix.length - 1) {
            if (b < i) {
              smaller(a - 1)(b) = matrix(a)(b)
            } else if (b > i) {
              smaller(a - 1)(b - 1) = matrix(a)(b)
            }
          }
        }
        s = changeSign(i)
        sum = mkOp("+", sum, mkOp("*", mkOp("*", Lit(GDouble(s)), matrix(0)(i)), (determinant(smaller)))) //recursive step: determinant of larger determined by smaller.
      }
      sum //returns determinant value. once stack is finished, returns final determinant.
    }
  }
}