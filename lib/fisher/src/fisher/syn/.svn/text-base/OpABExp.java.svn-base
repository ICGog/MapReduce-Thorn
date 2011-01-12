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
public  class OpABExp extends Cmd implements ProcMember {
 public  AssignTarget target; 
 public  OpAB opab; 
 public  Cmd amount; 
public OpABExp(Token start, Token end, AssignTarget target, OpAB opab, Cmd amount){
    super(start, end);
    this.target = target;
    if(target != null) children.add(target);
    this.opab = opab;

    this.amount = amount;
    if(amount != null) children.add(amount);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((OpABExp)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((OpABExp)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((OpABExp)this, arg);
}






public String details(){
return "OpABExp("
    + "target=" + Syntax.detailsome(this.target)
    + " "
    + "opab=" + Syntax.detailsome(this.opab)
    + " "
    + "amount=" + Syntax.detailsome(this.amount)
    +")";
}
public static String detstr(
Object target, Object opab, Object amount
) {
return "OpABExp(" + 
"target=" + target+" "+"opab=" + opab+" "+"amount=" + amount
+ ")";}
public String toString() {return  "" + target + opab + amount ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.target; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (target) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.opab; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (opab) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.amount; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (amount) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : target
              if (target != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : opab
              // 2 : amount
              if (amount != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.target = (AssignTarget) newval; break;
		 case 1: this.opab = (OpAB) newval; break;
		 case 2: this.amount = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "OpABExp";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new OpABExp(start, end, 
(target == null ? null : (AssignTarget)target.internalDeepCopy(start,end)), opab, 
(amount == null ? null : (Cmd)amount.internalDeepCopy(start,end)));

return copy;
}

public boolean explike(){return true;}
}