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
public  class PatRange extends Pat implements ProcMember {
 public  Pat low; 
 public  Pat high; 
public PatRange(Token start, Token end, Pat low, Pat high){
    super(start, end);
    this.low = low;
    if(low != null) children.add(low);
    this.high = high;
    if(high != null) children.add(high);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatRange)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatRange)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatRange)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((PatRange)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatRange)this, arg);
}


public String details(){
return "PatRange("
    + "low=" + Syntax.detailsome(this.low)
    + " "
    + "high=" + Syntax.detailsome(this.high)
    +")";
}
public static String detstr(
Object low, Object high
) {
return "PatRange(" + 
"low=" + low+" "+"high=" + high
+ ")";}
public String toString() {return  low + ".." + high ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.low; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (low) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.high; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (high) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : low
              if (low != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : high
              if (high != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.low = (Pat) newval; break;
		 case 1: this.high = (Pat) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatRange";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatRange(start, end, 
(low == null ? null : (Pat)low.internalDeepCopy(start,end)), 
(high == null ? null : (Pat)high.internalDeepCopy(start,end)));

return copy;
}

public boolean explike(){return true;}
}