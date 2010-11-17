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
public  class Assign extends Cmd implements ProcMember {
public final List<AssignTarget> lhs;
public final List<Cmd> rhs;
public Assign(Token start, Token end, List<AssignTarget> lhs, List<Cmd> rhs){
    super(start, end);
    this.lhs = lhs;
for(Object o : lhs){children.add((Syntax)o);}
    this.rhs = rhs;
for(Object o : rhs){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Assign)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Assign)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Assign)this, arg);
}






public String details(){
return "Assign("
    + "lhs=" + "["+Syntax.sepDetails(this.lhs, ",")+"]"
    + " "
    + "rhs=" + "["+Syntax.sepDetails(this.rhs, ",")+"]"
    +")";
}
public static String detstr(
Object lhs, Object rhs
) {
return "Assign(" + 
"lhs=" + lhs+" "+"rhs=" + rhs
+ ")";}
public String toString() {return sep(lhs, ", ") + " := " + sep(rhs, ", ");}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.lhs.get(index); ; break;
                     case 1: ret = this.rhs.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : lhs
              for(int i = 0; i < this.lhs.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              // 1 : rhs
              for(int i = 0; i < this.rhs.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.lhs.set(index, (AssignTarget) newval); break;
		 case 1: this.rhs.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Assign";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Assign(start, end, (List<AssignTarget>)(deepCopyList(lhs, start, end)), (List<Cmd>)(deepCopyList(rhs, start, end)));

return copy;
}

public boolean explike(){return true;}
}