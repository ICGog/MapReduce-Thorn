package fisher.syn;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import java.util.*;
import fisher.syn.visitor.*;
import fisher.syn.*;
import fisher.statics.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
import fisher.parser.Token;
import fisher.syn.chart.*;
import fisher.runtime.*;
import fisher.statics.purity.PurityStatus;
public  class AssignToFieldOfSubscripted extends AssignTarget implements ProcMember {
 public  Cmd array; 
public final List<Cmd> subscripts;
 public  Id field; 
public AssignToFieldOfSubscripted(Token start, Token end, Cmd array, List<Cmd> subscripts, Id field){
    super(start, end);
    this.array = array;
    if(array != null) children.add(array);
    this.subscripts = subscripts;
for(Object o : subscripts){children.add((Syntax)o);}
    this.field = field;
    if(field != null) children.add(field);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AssignToFieldOfSubscripted)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AssignToFieldOfSubscripted)this, arg);
}



public <ARG, RET,  EXN extends Exception> RET accept(VisitAssignTarget<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AssignToFieldOfSubscripted)this, arg);
}



public String details(){
return "AssignToFieldOfSubscripted("
    + "array=" + Syntax.detailsome(this.array)
    + " "
    + "subscripts=" + "["+Syntax.sepDetails(this.subscripts, ",")+"]"
    + " "
    + "field=" + Syntax.detailsome(this.field)
    +")";
}
public static String detstr(
Object array, Object subscripts, Object field
) {
return "AssignToFieldOfSubscripted(" + 
"array=" + array+" "+"subscripts=" + subscripts+" "+"field=" + field
+ ")";}
public String toString() {return  array + "(" + sep(subscripts, ", ")  + ")" + "." + field;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.array; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (array) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.subscripts.get(index); ; break;
                     case 2: ret = this.field; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (field) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : array
              if (array != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : subscripts
              for(int i = 0; i < this.subscripts.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : field
              if (field != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.array = (Cmd) newval; break;
		 case 1: this.subscripts.set(index, (Cmd) newval); break;
		 case 2: this.field = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "AssignToFieldOfSubscripted";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new AssignToFieldOfSubscripted(start, end, 
(array == null ? null : (Cmd)array.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(subscripts, start, end)), 
(field == null ? null : (Id)field.internalDeepCopy(start,end)));

return copy;
}


}