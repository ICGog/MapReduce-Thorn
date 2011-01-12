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
public  class QueryListComprehension extends QueryAbstract implements ProcMember {
 public  Cmd exp; 
 public  boolean isAppended; 
public QueryListComprehension(Token start, Token end, List<QueryControl> controls, Cmd actualCode, Cmd exp, boolean isAppended){
    super(start, end, controls, actualCode);
    this.exp = exp;

    this.isAppended = isAppended;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryListComprehension)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryListComprehension)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryListComprehension)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryAbstract<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryListComprehension)this, arg);
}
public String details(){
return "QueryListComprehension("
    + "controls=" + "["+Syntax.sepDetails(this.controls, ",")+"]"
    + " "
    + "exp=" + Syntax.detailsome(this.exp)
    + " "
    + "isAppended=" + Syntax.detailsome(this.isAppended)
    +")";
}
public static String detstr(
Object controls, Object exp, Object isAppended
) {
return "QueryListComprehension(" + 
"controls=" + controls+" "+"exp=" + exp+" "+"isAppended=" + isAppended
+ ")";}
public String toString() {return 
     "%[" 
     + exp 
     + (isAppended ? "\u2026" : "")
     + super.toString()
     + "]"
   ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.controls.get(index); ; break;
                     case 1: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.exp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (exp) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.isAppended; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isAppended) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : controls
              // 1 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : exp
              // 3 : isAppended
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.controls.set(index, (QueryControl) newval); break;
		 case 1: this.actualCode = (Cmd) newval; break;
		 case 2: this.exp = (Cmd) newval; break;
		 case 3: this.isAppended = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryListComprehension";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryListComprehension(start, end, (List<QueryControl>)(deepCopyList(controls, start, end)), 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)), 
(exp == null ? null : (Cmd)exp.internalDeepCopy(start,end)), isAppended);

return copy;
}


}