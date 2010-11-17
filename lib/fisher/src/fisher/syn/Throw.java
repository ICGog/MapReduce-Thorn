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
public  class Throw extends Cmd implements ProcMember {
 public  Cmd exn; 
public Throw(Token start, Token end, Cmd exn){
    super(start, end);
    this.exn = exn;
    if(exn != null) children.add(exn);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Throw)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Throw)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Throw)this, arg);
}






public String details(){
return "Throw("
    + "exn=" + Syntax.detailsome(this.exn)
    +")";
}
public static String detstr(
Object exn
) {
return "Throw(" + 
"exn=" + exn
+ ")";}
public String toString() {return  "throw " + exn + ";" ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.exn; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (exn) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : exn
              if (exn != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.exn = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Throw";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Throw(start, end, 
(exn == null ? null : (Cmd)exn.internalDeepCopy(start,end)));

return copy;
}


}