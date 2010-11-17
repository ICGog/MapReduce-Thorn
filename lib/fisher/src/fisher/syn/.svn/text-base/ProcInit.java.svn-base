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
public  class ProcInit extends Syntax implements ProcMember {
 public  Cmd initcode; 
public ProcInit(Token start, Token end, Cmd initcode){
    super(start, end);
    this.initcode = initcode;
    if(initcode != null) children.add(initcode);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ProcInit)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ProcInit)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ProcInit)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ProcInit)this, arg);
}
public String details(){
return "ProcInit("
    + "initcode=" + Syntax.detailsome(this.initcode)
    +")";
}
public static String detstr(
Object initcode
) {
return "ProcInit(" + 
"initcode=" + initcode
+ ")";}
public String toString() {return  "initially{" + initcode + "} " ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.initcode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (initcode) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : initcode
              if (initcode != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.initcode = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ProcInit";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ProcInit(start, end, 
(initcode == null ? null : (Cmd)initcode.internalDeepCopy(start,end)));

return copy;
}


}