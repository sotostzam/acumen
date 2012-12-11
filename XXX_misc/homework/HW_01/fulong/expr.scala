abstract class Expression
case class Var (val x:String) extends Expression
case class Const(val i:Int) extends Expression
case class Plus(val e1:Expression,val e2:Expression) extends Expression
case class Minus(val e1:Expression,val e2:Expression) extends Expression
case class Repeat(val x1:String,val i1:Int,val i2:Int,val x2:String,val i3:Int,val e:Expression) extends Expression
object expr {

def find_value(varlist:List[(String,Int)],varname:String):Int={
if (varlist==Nil) 0
else if (varlist.head._1==varname)varlist.head._2
else find_value(varlist.tail,varname)}

def update_value(varlist:List[(String,Int)],varname:String,varvalue:Int):List[(String,Int)]={
if (varlist==Nil) Nil
else if (varlist.head._1==varname)(varlist.head._1,varvalue)::varlist.tail
else varlist.head::update_value(varlist.tail,varname,varvalue)}

def evaluate_value(e:Expression,varlist:List[(String,Int)]):Int=
{
 e match {
 case Var(x)=>find_value(varlist,x)
 case Const(i)=>i
 case Plus(e1,e2)=>evaluate_value(e1,varlist)+evaluate_value(e2,varlist)
 case Minus(e1,e2)=>evaluate_value(e1,varlist)-evaluate_value(e2,varlist)
 case Repeat(x1,i1,i2,x2,i3,e1)=>if (i1>i2)i3 else 
   {var new_i1=i1+1 
    var new_varlist1=update_value(varlist,x1,i1) 
    var new_varlist2=update_value(new_varlist1,x2,i3) 
    var new_i3=evaluate_value(e1,new_varlist2)
    evaluate_value(Repeat(x1,new_i1,i2,x2,new_i3,e1),new_varlist2)
   }
 }
}
}
