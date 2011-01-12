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
public  class ComparisonBit extends Syntax implements ProcMember {
 public  ComparisonOp comparison; 
 public  Cmd to; 
public ComparisonBit(Token start, Token end, ComparisonOp comparison, Cmd to){
    super(start, end);
    this.comparison = comparison;

    this.to = to;
    if(to != null) children.add(to);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ComparisonBit)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ComparisonBit)this, arg);
}







public String details(){
return "ComparisonBit("
    + "comparison=" + Syntax.detailsome(this.comparison)
    + " "
    + "to=" + Syntax.detailsome(this.to)
    +")";
}
public static String detstr(
Object comparison, Object to
) {
return "ComparisonBit(" + 
"comparison=" + comparison+" "+"to=" + to
+ ")";}
public String toString() {return " " + comparison + " " + to;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.comparison; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (comparison) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.to; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (to) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : comparison
              // 1 : to
              if (to != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.comparison = (ComparisonOp) newval; break;
		 case 1: this.to = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ComparisonBit";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ComparisonBit(start, end, comparison, 
(to == null ? null : (Cmd)to.internalDeepCopy(start,end)));

return copy;
}


}