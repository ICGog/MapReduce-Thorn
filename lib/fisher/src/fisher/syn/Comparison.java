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
public  class Comparison extends Cmd implements ProcMember {
 public  Cmd first; 
public final List<ComparisonBit> rest;
public Comparison(Token start, Token end, Cmd first, List<ComparisonBit> rest){
    super(start, end);
    this.first = first;
    if(first != null) children.add(first);
    this.rest = rest;
for(Object o : rest){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Comparison)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Comparison)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Comparison)this, arg);
}






public String details(){
return "Comparison("
    + "first=" + Syntax.detailsome(this.first)
    + " "
    + "rest=" + "["+Syntax.sepDetails(this.rest, ",")+"]"
    +")";
}
public static String detstr(
Object first, Object rest
) {
return "Comparison(" + 
"first=" + first+" "+"rest=" + rest
+ ")";}
public String toString() {return first + sep(rest, "");}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.first; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (first) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.rest.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : first
              if (first != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : rest
              for(int i = 0; i < this.rest.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.first = (Cmd) newval; break;
		 case 1: this.rest.set(index, (ComparisonBit) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Comparison";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Comparison(start, end, 
(first == null ? null : (Cmd)first.internalDeepCopy(start,end)), (List<ComparisonBit>)(deepCopyList(rest, start, end)));

return copy;
}

public boolean explike(){return true;}
}