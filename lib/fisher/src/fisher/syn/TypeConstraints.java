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
public  class TypeConstraints extends Syntax implements ProcMember {
public final List<TypeConstraint> constraints;
public TypeConstraints(Token start, Token end, List<TypeConstraint> constraints){
    super(start, end);
    this.constraints = constraints;
for(Object o : constraints){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TypeConstraints)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((TypeConstraints)this, arg);
}







public String details(){
return "TypeConstraints("
    + "constraints=" + "["+Syntax.sepDetails(this.constraints, ",")+"]"
    +")";
}
public static String detstr(
Object constraints
) {
return "TypeConstraints(" + 
"constraints=" + constraints
+ ")";}
public String toString() {return  
       (constraints.isEmpty() 
        ? "" 
        : ":" + sep(constraints, " & "));}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.constraints.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : constraints
              for(int i = 0; i < this.constraints.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.constraints.set(index, (TypeConstraint) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "TypeConstraints";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new TypeConstraints(start, end, (List<TypeConstraint>)(deepCopyList(constraints, start, end)));

return copy;
}


        public List<Id> ids() {
          List<Id> ids = new ArrayList<Id>(constraints.size());
          for(TypeConstraint tc : constraints) {
             ids.add(tc.typename);
          }
          return ids;
        }
    
}