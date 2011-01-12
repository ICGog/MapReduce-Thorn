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
public  class AssignToSubscripted extends AssignTarget implements ProcMember {
 public  Cmd array; 
public final List<Cmd> subscripts;
public AssignToSubscripted(Token start, Token end, Cmd array, List<Cmd> subscripts){
    super(start, end);
    this.array = array;
    if(array != null) children.add(array);
    this.subscripts = subscripts;
for(Object o : subscripts){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AssignToSubscripted)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AssignToSubscripted)this, arg);
}



public <ARG, RET,  EXN extends Exception> RET accept(VisitAssignTarget<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AssignToSubscripted)this, arg);
}



public String details(){
return "AssignToSubscripted("
    + "array=" + Syntax.detailsome(this.array)
    + " "
    + "subscripts=" + "["+Syntax.sepDetails(this.subscripts, ",")+"]"
    +")";
}
public static String detstr(
Object array, Object subscripts
) {
return "AssignToSubscripted(" + 
"array=" + array+" "+"subscripts=" + subscripts
+ ")";}
public String toString() {return  array + "(" + sep(subscripts, ", ") + ")" ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.array; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (array) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.subscripts.get(index); ; break;

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
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.array = (Cmd) newval; break;
		 case 1: this.subscripts.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "AssignToSubscripted";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new AssignToSubscripted(start, end, 
(array == null ? null : (Cmd)array.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(subscripts, start, end)));

return copy;
}


}