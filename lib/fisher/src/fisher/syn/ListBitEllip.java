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
public  class ListBitEllip extends ListBit implements ProcMember {
 public  Cmd exp; 
public ListBitEllip(Token start, Token end, Cmd exp){
    super(start, end);
    this.exp = exp;
    if(exp != null) children.add(exp);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ListBitEllip)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ListBitEllip)this, arg);
}







public String details(){
return "ListBitEllip("
    + "exp=" + Syntax.detailsome(this.exp)
    +")";
}
public static String detstr(
Object exp
) {
return "ListBitEllip(" + 
"exp=" + exp
+ ")";}
public String toString() {return  exp + "\u2026" ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.exp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (exp) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : exp
              if (exp != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.exp = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ListBitEllip";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ListBitEllip(start, end, 
(exp == null ? null : (Cmd)exp.internalDeepCopy(start,end)));

return copy;
}


}