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
public  class ModArgBinding extends Syntax implements ProcMember {
 public  Id formal; 
 public  QualName actual; 
public ModArgBinding(Token start, Token end, Id formal, QualName actual){
    super(start, end);
    this.formal = formal;
    if(formal != null) children.add(formal);
    this.actual = actual;
    if(actual != null) children.add(actual);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModArgBinding)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModArgBinding)this, arg);
}







public String details(){
return "ModArgBinding("
    + "formal=" + Syntax.detailsome(this.formal)
    + " "
    + "actual=" + Syntax.detailsome(this.actual)
    +")";
}
public static String detstr(
Object formal, Object actual
) {
return "ModArgBinding(" + 
"formal=" + formal+" "+"actual=" + actual
+ ")";}
public String toString() {return 
    formal + "=" + actual
    ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.formal; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (formal) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.actual; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actual) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : formal
              if (formal != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : actual
              if (actual != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.formal = (Id) newval; break;
		 case 1: this.actual = (QualName) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ModArgBinding";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ModArgBinding(start, end, 
(formal == null ? null : (Id)formal.internalDeepCopy(start,end)), 
(actual == null ? null : (QualName)actual.internalDeepCopy(start,end)));

return copy;
}


}