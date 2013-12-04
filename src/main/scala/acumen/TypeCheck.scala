package acumen

import Errors._

import scala.collection._

object TypeCheck {

  //*********************************************************
  // Final Type Check Result
  //

  val EL_NONE = 0
  val EL_UNUSED = 1
  val EL_DYNAMIC = 2
  val EL_ERROR = 3
  val EL_FAIL = 4
  def errorLevelStr(errorLevel: Int) = errorLevel match {
    case EL_NONE => "No problems."
    case EL_UNUSED => "Unused classes (possible dynamic type in unused classes)."
    case EL_DYNAMIC => "Unification failures (dynamic types required)."
    case EL_ERROR => "Type errors."
    case EL_FAIL => "Type checked failed, likely due to an internal error."
    case _ => "<Invalid error level.>"
  }

}

class TypeCheck(prog0: Prog) {

  import TypeCheck._

  //*********************************************************
  // Nonfinalized Types
  //

  sealed abstract class NonFinalizedType extends TypeLike {
    override def finalized = false
    def duplicate : TypeLike // so I don't understand how special clone
                             // method is suppose to work so lets use
                             // something else -- kevina
  }

  sealed abstract class NonFinalizedTypeObject extends NonFinalizedType {
    override def duplicate = this
  }

  case object UnknownType extends NonFinalizedTypeObject // type yet to be determined (top)

  case object ZeroType extends NonFinalizedTypeObject with NumericLike 
  case object IntTypeNF extends NonFinalizedTypeObject with NumericLike

  class ClassTypeNF(cn: ClassName = null) extends NonFinalizedType {
    override def duplicate = {
      var res = new ClassTypeNF()
      res.classNames ++= this.classNames
      res.nonBaseFieldsAccessed = this.nonBaseFieldsAccessed
      res
    }
    override def classLike = true
    override def classSubType = {
      if (classNames.size == 1)       NamedClass(classNames(0))
      else if (nonBaseFieldsAccessed) DynamicClass
      else                            BaseClass
    }
    val classNames = collection.mutable.Buffer.empty[ClassName]
    if (cn != null) classNames += cn
    var nonBaseFieldsAccessed = false
    val _types = new TypeEnv {
      def get(name: Name) = {
        val typ = baseClassTypes.get(name) getOrElse {
          nonBaseFieldsAccessed = true
          val typs = classNames.map{cn => classEnvs(cn).get(name) getOrElse {
            if (finalize_) DynamicType
            else           UnknownType
          }}
          //println("Dynamic Class field \"" + name + "\" types:" + typs)
          typs.reduce{(x,y) => unifyTypeSpecial(x,y)}
        }
        //println("Dynamic Class field \"" + name + "\" typ:" + typ)
        Some(typ)
      }
      def update(name: Name, typ: TypeLike) = {
        if (baseClassTypes.get(name).isEmpty) {
          nonBaseFieldsAccessed = true
          classNames.foreach{cn => classEnvs(cn).update(name, typ)}
        }
      }
    }
    def addClass(cn: ClassName) {
      if (!classNames.exists{cn0 => cn == cn0}) {
        //typeError("invalidated due adding a new class to dynamic class type")
        invalidated = true
        classNames += cn
      }
    }
  }

  def getAssumedType0(typ: NumericLike) : SomeNumericType = typ match {
    case ZeroType|IntTypeNF => IntType
    case t:SomeNumericType  => t
  }

  def getAssumedType(typ: TypeLike) : TypeLike = typ match {
    case ZeroType|IntTypeNF  => IntType
    case SeqType(st)         => SeqType(st.map{t => getAssumedType(t)})
    case _                   => typ
  }

  //*********************************************************
  // Misc
  //

  type Env = TypeEnv

  class ClassEnv(var _types : mutable.Map[Name, TypeLike]) extends Env {
    def update(name: Name, typ: TypeLike) = _types.update(name,typ)
    def get(name: Name) = _types.get(name)
  }

  val FIXME = null


  //*********************************************************
  // State
  //

  val classEnvs = mutable.Map.empty[ClassName, ClassEnv]

  val cantCheck = mutable.Map.empty[ClassName,ClassDef]
  val mayCheck = mutable.Queue.empty[ClassDef]
  var checking: ClassDef = null
  var checked = mutable.Queue.empty[ClassDef]
  def mayCheckEnqueue(cn: ClassName) {
    cantCheck.remove(cn) match {
      case Some(cd) => mayCheck.enqueue(cd)
      case None     =>
    }
  }


  var invalidated = false;
  var finalize_ = false;

  var errorLevel = 0
  def setErrorLevel(e: Int) {
    if (e > errorLevel) errorLevel = e
  }

  var context = mutable.Stack.empty[() => String]
  def resetContext (newContext: => String) : Unit = {
    context.clear
    context.push{() => newContext}
  }
  def pushContext (newContext: => String) : Unit = 
    context.push{() => newContext}
  def popContext = context.pop

  var silent = false


  //*********************************************************
  // Specials
  //

  val baseClassTypes = new ClassEnv(mutable.Map.empty[Name, TypeLike])
  addSpecialFields(ClassType(BaseClass), baseClassTypes)
  finalizeFields(baseClassTypes._types)

  val simulator = ClassDef(ClassName("Simulator"), Nil, Nil, Nil);
  {
    simulator._types = mutable.Map.empty[Name, TypeLike];
    val env = new ClassEnv(simulator._types)
    classEnvs.update(simulator.name, env)
    env.update(Name("time", 0), NumericType)
    env.update(Name("timeStep", 0), NumericType)
    env.update(Name("endTime", 0), NumericType)
    env.update(Name("resultType", 0), ResultTypeType)
    addSpecialFields(ClassType(NamedClass(simulator.name)), env)
  }

  def addSpecialFields(ct: ClassType, env: Env) {
    env.update(Name("self", 0), ct)
    env.update(Name("parent", 0), new ClassTypeNF())
    env.update(Name("children", 0), new SeqType(DynamicSize(new ClassTypeNF())))
    env.update(Name("seed1", 0), IntType)
    env.update(Name("seed2", 0), IntType)
    env.update(Name("className",0), StrType)
  }


  //*********************************************************
  // Initialization
  //

  // Make a copy of the AST and reset the types for each class
  val prog = new ASTMap {
    override def mapClassDef(c0: ClassDef) = {
      val c = super.mapClassDef(c0) // make a copy
      c._types = mutable.Map.empty[Name, TypeLike]
      val env = new ClassEnv(c._types)
      classEnvs.update(c.name, env)
      addSpecialFields(ClassType(NamedClass(c.name)), env)
      c.fields.foreach { n => updateFieldType(env, n, UnknownType) }
      c.priv.foreach { case Init(n, _) => updateFieldType(env, n, UnknownType) }
      c
    }
  }.mapProg(prog0)

  getClassDef(ClassName("Main")).orNull._types.update(Name("simulator",0), ClassType(NamedClass(ClassName("Simulator"))))


  //*********************************************************
  // Entry point
  //

  def run() : (Prog, Int) = {
    silent = false
    var count = 0
    do {
      invalidated = false
      typeCheckIter()
      if (invalidated) {
        //println(Pretty.pprint(prog))
        println("*** AGAIN **** (Silent Mode On)")
      }
      count += 1
      silent = true
    } while (invalidated && count < 12)
    if (invalidated) {
      println("*** TO MANY PASSES: GIVING UP ***")
      setErrorLevel(EL_FAIL)
      invalidated = false
    }
    finalize_ = true
    prog.defs.foreach{c => finalizeFields(c._types)}
    silent = false
    println("*** FINAL PASS ***")
    typeCheckIter()
    (prog, errorLevel)
  }

  def typeCheckIter() {
    // pre and post: mayCheck, checked, checking empty or null
    cantCheck ++= prog.defs.map{c => (c.name, c)}
    mayCheckEnqueue(ClassName("Main"))
    while (!mayCheck.isEmpty) {
      typeCheckClass()
    }
    cantCheck.foreach { case (_,cd) =>
      if (!silent)
        println("Warning: class \"" + cd.name.x + "\" never used, forcing type check")
      setErrorLevel(EL_UNUSED)
      //cd.fields.foreach { n => updateFieldType(cd._types, n, ZeroType) }
      mayCheck.enqueue(cd)
    }
    cantCheck.clear
    while (!mayCheck.isEmpty) {
      typeCheckClass()
    }
    // if cantCheck is empty emit error about unused programs
  }


  //*********************************************************
  // Methods for walking the tree and updaing the type
  //

  // Typed check the class at the head of the mayCheck queue
  // assumes the types of the fileds are known
  def typeCheckClass() {
    val cd = mayCheck.dequeue
    //println("now checking: " + cd.name.x)
    
    val env = classEnvs(cd.name)
    checking = cd
    cd.priv.foreach{case init@Init(name, rhs) => 
      resetContext("  in " + Pretty.pprint[Init](init))
      rhs match {
        case NewRhs(Var(n), fields) => 
          val cn = ClassName(n.x)
          typeCheckCreate(cn, fields, cd)
          setFieldType(env, name, ClassType(NamedClass(cn)))
        case ExprRhs(e) => 
          val typ = typeCheckExpr(e, env)
          setFieldType(env, name, typ)
      }
    }
    typeCheckActions(cd.body, cd)
    checking = null
    checked.enqueue(cd)
  }

  def typeCheckActions(l: List[Action],  parent: ClassDef) {
    l.foreach{el => typeCheckAction(el, parent)}
  }

  def typeCheckAction(a: Action, parent: ClassDef) {
    val env = classEnvs(parent.name)
    resetContext("  in " + Pretty.pprint[Action](a))
    a match {
      case IfThenElse(c, a1, a2) =>
        typeCheckExpr(c, BoolType, env)
        typeCheckActions(a1, parent)
        typeCheckActions(a2, parent)
      case ForEach(i, l, b) =>
        /* FIXME */
      case Switch(s, cls) =>
        typeCheckExpr(s, env)
        cls.foreach{case Clause(lhs, assertion, actions) => 
          unifyTypeForCmp(s._type, toType(lhs))
          typeCheckExpr(assertion, BoolType, env) 
          typeCheckActions(actions, parent)
        }
      case Discretely(da) =>
        da match {
          case Assign(lhs, rhs) =>
            typeCheckExpr(lhs, env)
            typeCheckExpr(rhs, env)
            unifyTypeWithLvalue(lhs, rhs._type)
          case Create(lhs, Var(name), args) =>
            val typ = typeCheckCreate(ClassName(name.x), args, parent)
            lhs match {
              case Some(e) => 
                typeCheckExpr(e, env)
                unifyTypeWithLvalue(e, typ)
              case None => 
            }
          case Elim(e) => 
            typeCheckExpr(e, ClassType(BaseClass), env)
          case Move(obj, newParent) =>
            typeCheckExpr(obj, ClassType(BaseClass), env)
            typeCheckExpr(newParent, ClassType(BaseClass), env)
        }
      case Continuously(ca) =>
        //println("cont: " + ca);
        val (lhs,rhs) = ca match {
          case Equation(lhs, rhs) => (lhs,rhs)
          case EquationI(lhs, rhs) => (lhs,rhs)
          case EquationT(lhs, rhs) => (lhs,rhs)
        }
        typeCheckExpr(lhs, env)
        typeCheckExpr(rhs, env)
        unifyTypeWithLvalue(lhs, expectNumeric(rhs._type))
    }
  }

  def expectNumeric(typ: TypeLike) : TypeLike = {
    //println("expectNumeric: " + typ);
    val res = typ match {
      case (typ:NumericLike)  => NumericType
      case SeqType(st)        => SeqType(st.map{t => expectNumeric(t)})
      case _                  => typ
    }
    //println("expectNumeric= " + res)
    res
  }

  def typeCheckCreate(cn: ClassName, args: List[Expr], parent: ClassDef) : TypeLike = {
    val env = classEnvs(parent.name)
    val cd = getClassDef(cn) getOrElse {return typeError(EL_ERROR, "class not found: " + cn.x)}
    args.indices.foreach{i =>
      val field = cd.fields(i)
      val typ   = typeCheckExpr(args(i), env)
      pushContext("  while updating " + cd.name.x + "." + cd.fields(i).x + " with " + Pretty.pprint[Expr](args(i)))
      updateFieldType(classEnvs(cd.name), field, typ)
      popContext
    }
    updateFieldType(classEnvs(cd.name), Name("parent", 0), ClassType(NamedClass(parent.name)))
    updateFieldType(env, Name("children", 0), SeqType(DynamicSize(ClassType(NamedClass(cn)))))
    mayCheckEnqueue(cn)
    val typ = ClassType(NamedClass(cn))
    //unifyType(_types("children"), ClassType(NamedClass(cn)))
    typ
  }

  def typeCheckExpr(e: Expr, expect: Option[TypeLike], env: Env) : TypeLike = {
    expect match {
      case Some (t) => typeCheckExpr(e,t,env)
      case None     => typeCheckExpr(e,env)
    }
  }

  def typeCheckExpr(e: Expr, expect: TypeLike, env: Env) : TypeLike = {
    val typ = typeCheckExpr(e, env)
    pushContext("  in " + Pretty.pprint[Expr](e))
    unifyType(expect, typ)
    popContext
    typ
  }

  def typeCheckExpr(e: Expr, env: Env) : TypeLike = {
    pushContext("  in " + Pretty.pprint[Expr](e))
    def invalidOp(msg: String, op: String) = typeError(EL_ERROR, msg + ": " + op)
    e._lvalue = None
    e._type = e match {
      case Lit(gv) => 
        toType(gv)
      case Var(name) => 
        e._lvalue = Some((env, name))
        env.get(name) getOrElse typeError(EL_ERROR, "could not find variable: " + name)
      case Dot(obj, field) =>
        //println("??" + e)
        typeCheckExpr(obj, env)
        val (env0, typ) = lookupField(obj._type, field)
        e._lvalue = Some((env0, field))
        typ
      case Op(Name(op,_), x::Nil) => 
        typeCheckExpr(x, env)
        pushContext("  in op with type " + x._type)
        val res = (op, getAssumedType(x._type)) match {       
          case (_, DynamicType) =>
            DynamicType
          case (_, UnknownType) =>
            UnknownType
          case ("not", BoolType) => 
            BoolType
          case ("abs"|"-", IntType) =>
            IntType
          case ("round", NumericType) =>
            NumericType
          case (_, IntType|NumericType) =>
            NumericType
          case (_, SeqType(st)) if st.isNumeric => op match {
            case "norm" => NumericType
            case _      => invalidOp("invalid vector op", op)
          }
          case (_, _) => invalidOp("invalid or unknown unary op", op)
        }
        popContext
        res
      case Op(Name(op, _), x::y::Nil) => 
        typeCheckExpr(x, env)
        typeCheckExpr(y, env)
        pushContext("  in op with types " + x._type + " and " + y._type)
        val res = (op, getAssumedType(x._type), getAssumedType(y._type)) match {
          case ("=="|"~=", _, _) => 
            // FIXME: check x type == y type are compatible
            BoolType
          case (_, DynamicType, _) | (_, _, DynamicType) =>
            DynamicType
          case (_, UnknownType, _) | (_, _, UnknownType) =>
            UnknownType
          case (_, BoolType, BoolType) =>
            op match {
              case "&&"|"||" => BoolType
              case _         => invalidOp("invalid binary boolean op", op)
            }
          case ("+"|"-"|"*"|"<<"|">>"|"&"|"|"|"%"|"xor", IntType, IntType) => 
            IntType
          case (_, IntType|NumericType, IntType|NumericType) => 
            op match {
              case ">="|"<="|"<"|">"            => BoolType
              case "+"|"-"|"*"|"^"|"/"|"atan2"  => NumericType
              case _                            => invalidOp("invalid numeric boolean op", op)
            }
          case (_, SeqType(st1), SeqType(st2)) =>
            if (st1.size != st2.size) 
              typeError(EL_ERROR, "binary vector op sizes do not match")
            else op match {
              case ".*"|"./"|".^"|"+"|"-"|"cross" => 
                if (st1.isNumeric && st2.isNumeric) SeqType(st1.map{_ => NumericType})
                else                                SeqType(st1.map{_ => DynamicType})
              case "dot" => 
                if (st1.isNumeric && st2.isNumeric) NumericType
                else                                DynamicType
              case _                              => 
                invalidOp("invalid vector binary op", op)
            }
          case (_, SeqType(st), IntType|NumericType) if st.isNumeric =>
            op match {
              case "+"|"*"|"/"|".^" => SeqType(st.map{_ => NumericType})
              case _                => invalidOp("invalid vector scalar op", op)
            }
          case (_, IntType|NumericType, SeqType(st)) if st.isNumeric =>
            op match {
              case "+"|"*" => SeqType(st.map{_ => NumericType})
              case _       => invalidOp("invalid scalar vector op", op)
            }
          case (_, SeqType(_), _) | (_, _, SeqType(_)) =>
            DynamicType
          case (_,x,y) => 
            typeError(EL_ERROR, "no match for binary op " + op + " with types " + x + " and " + y)
        }
        popContext
        res
      case Op(Name("_:_:_", _), s::d::e::Nil) =>
        typeCheckExpr(s, IntType, env)
        typeCheckExpr(d, IntType, env)
        typeCheckExpr(e, IntType, env)
        SeqType(DynamicSize(IntType))
      case ExprVector(l) =>
        val resTypes = l.map{ e => 
          typeCheckExpr(e,env)
        }
        SeqType(FixedSize(resTypes))
      case _ => 
        typeError(EL_ERROR, "unrecognized expr")
    }
    popContext
    e._type
  }

  def toType(gv: GroundValue) = gv match {
    case GInt(0) => ZeroType
    case GInt(_) => IntType
    case GDouble(_) => NumericType
    case GBool(_) => BoolType
    case GStr(_) => StrType
  }


  //*********************************************************
  // Unification
  //

  // will invalidate if required
  // if returning rhs will make sure to make a copy if required
  def unifyType(lhs: TypeLike, rhs: TypeLike) : Option[TypeLike] = {
    pushContext("  while unifying " + lhs + " with " + rhs)
    //println("unify: " + lhs + " vs " + rhs)
    val res = (lhs, rhs) match {
      // simple case, types match
      case (x:Type, y:Type) if x == y => 
        Some(lhs)

      case (DynamicType, _) =>
        Some(lhs)

      // special case with UnknownType (top)
      case (_, UnknownType) =>
        Some(lhs)
      case (UnknownType, _) =>
        //typeError("invalidated due to UnknownType")
        invalidated = true
        Some(unFinalizeOrCopyType(rhs))

      // similar special case with ZeroType now that we are there are no unknowns
      case (_, ZeroType) =>
        Some(lhs)
      case (ZeroType, _) =>
        //typeError("invalidated due to ZeroType: " + lhs + " with " + rhs)
        invalidated = true
        Some(unFinalizeOrCopyType(rhs))

      // numeric type inference
      case (IntTypeNF, IntType|IntTypeNF) =>
        Some(lhs)
      case (IntTypeNF, _) if rhs.numericLike =>  
        //typeError("invalidated due to IntTypeNF (with " + rhs + ")")
        invalidated = true
        Some(unFinalizeType(rhs))
      case (NumericType, IntType|IntTypeNF) => 
        Some(lhs)

      case (ClassType(BaseClass|DynamicClass),_) if rhs.classLike => 
        Some(lhs)
      case (ClassType(st@NamedClass(x)), _) if st == rhs.classSubType =>
        Some(lhs)
      case (lhs:ClassTypeNF, ClassType(BaseClass)) =>
        Some(lhs)
      case (lhs:ClassTypeNF, ClassType(NamedClass(cn))) =>
        lhs.addClass(cn)
        Some(lhs)
      case (lhs:ClassTypeNF, rhs:ClassTypeNF) =>
        rhs.classNames.foreach{cn => lhs.addClass(cn)}
        Some(lhs)

      // anything may be converted to a string
      case (StrType, _) if rhs.numericLike =>
        Some(lhs)

      case (SeqType(xst), SeqType(yst)) if xst.size == yst.size => 
        // Possible be less strict, maybe need VectorTypeNF
        Some(SeqType(xst.zip(yst, {(a, b) => 
          unifyType(a,b) getOrElse 
            typeError(EL_DYNAMIC, "failed to unify sub vector type " + a + " with " + b);})))
      case (SeqType(DynamicSize(DynamicType)), SeqType(_)) =>
        Some(lhs)
      case (SeqType(_), SeqType(_)) =>
        typeError(EL_DYNAMIC, "failed to unify vector types " + lhs + " with " + rhs)
        Some(SeqType(DynamicSize(DynamicType)))

      //case (_, null) => throw new NullPointerException
      case _ => 
        None
    }
    popContext
    res
  }

  def unifyTypeWithLvalue(lhs: Expr, rhs: TypeLike) {
    lhs._lvalue match {
      case Some((env, name)) => 
        val typ = unifyType(lhs._type, rhs) getOrElse 
          typeError(EL_DYNAMIC, "failed to unify " + lhs._type + " with " + rhs)
        if (env != null) 
          env.update(name, typ)
        else
          println("oh no, no env on type: " + typ)
      case None => 
        //invalidated = true
        typeError(EL_ERROR, "expected lvalue")
    }
  }

  def unifyTypeSpecial(x: TypeLike, y: TypeLike) : TypeLike = {
    if (x == y)  x
    else {
      //println("Can't unifyTypeSpecial " + x + " with " + y)
      DynamicType
    }
  }

  def unifyTypeForCmp(x: TypeLike, y: TypeLike) = FIXME

  def unFinalizeType(typ: TypeLike) : TypeLike = typ match {
    case ClassType(BaseClass) => 
      new ClassTypeNF()
    case ClassType(NamedClass(cn)) =>
      new ClassTypeNF(cn)
    case IntType =>
      IntTypeNF
    case SeqType(st) =>
      SeqType(st map unFinalizeType)
    case _ =>
      typ
  }

  def unFinalizeOrCopyType(typ: TypeLike) : TypeLike = typ match {
    case t:Type => 
      unFinalizeType(t)
    case t:NonFinalizedType =>
      t.duplicate
  }


  //*********************************************************
  // Utility
  //

  def typeError(e: Int, msg: String) : TypeLike = {
    if (e == EL_DYNAMIC)
      invalidated = true
    if (!silent) {
      println("Error: " + msg)
      context.foreach{ el => 
        println(el())
      }
      if (checking != null)
        println("  in class: " + checking.name.x)
    }
    setErrorLevel(e)
    DynamicType
  }

  //case class ClassNotFound(cn: ClassName) extends Exception

  def getClassDef(cn: ClassName) : Option[ClassDef] = {
    cn match {
      case ClassName("Simulator") => 
        Some(simulator)
      case _ => 
        prog.defs.find{c => c.name == cn}
    }
  }

  // check for class type, adjust class type if necessary
  // return type of field if possible
  def lookupField(obj: TypeLike, n: Name) : (Env, TypeLike) = {
    obj match {
      case ClassType(BaseClass) => 
        (null,
         baseClassTypes.get(n) 
           getOrElse 
           typeError(EL_ERROR, "field not found: " + n + " in generic acumen object"))
      case ClassType(NamedClass(cn)) =>
        val env = classEnvs(cn)
        (env, 
         env.get(n) getOrElse typeError(EL_ERROR, "field not found: " + n + " in " + cn))
      case ClassType(DynamicClass) =>
        (null,
         baseClassTypes.get(n) getOrElse DynamicType)
      case typ:ClassTypeNF => 
        (typ._types,
         typ._types.get(n) getOrElse DynamicType)
      case DynamicType =>
        (null, DynamicType)
      case _ => (null, typeError(EL_ERROR, "non class type: " + obj))
    }
  }

  def setFieldType(env: Env, name: Name, typ: TypeLike) : Unit = {
    if (finalize_) // noop if finalizing
      return
    //env.update(name, unFinalizeType(typ))
    updateFieldType(env: Env, name: Name, typ: TypeLike)
  }
 
  def updateFieldType(env: Env, name: Name, typ: TypeLike) : Unit = {
    env.get(name) match {
      case Some(expect) => env.update(name, unifyType(expect, typ) 
                                               getOrElse 
                                               typeError(EL_DYNAMIC, "failed to unify " + expect + " with " + typ))
      case None         => env.update(name, unFinalizeType(typ))
    }
  }

  def finalizeType(typ: TypeLike) : TypeLike = typ match {
    case c:ClassTypeNF      => c.classSubType match {
      case DynamicClass => c // Nonfinalized dynamic class type currently unimplemented
      case _            => ClassType(c.classSubType)
    }
    case ZeroType|IntTypeNF => IntType
    //case UnknownType        => DynamicType
    case SeqType(st) => SeqType(st.map{t => finalizeType(t)})
    case _ => typ
  }

  def finalizeFields(_types: scala.collection.mutable.Map[Name, TypeLike]) {
    _types.transform{(name, typ) => finalizeType(typ)}
  }

}
