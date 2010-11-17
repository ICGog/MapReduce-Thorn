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
public  class ProcBody extends Syntax implements ProcMember {
 public  Cmd bodycode; 
public ProcBody(Token start, Token end, Cmd bodycode){
    super(start, end);
    this.bodycode = bodycode;
    if(bodycode != null) children.add(bodycode);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ProcBody)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ProcBody)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ProcBody)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ProcBody)this, arg);
}
public String details(){
return "ProcBody("
    + "bodycode=" + Syntax.detailsome(this.bodycode)
    +")";
}
public static String detstr(
Object bodycode
) {
return "ProcBody(" + 
"bodycode=" + bodycode
+ ")";}
public String toString() {return  "body{" + bodycode + "} " ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.bodycode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (bodycode) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : bodycode
              if (bodycode != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.bodycode = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ProcBody";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ProcBody(start, end, 
(bodycode == null ? null : (Cmd)bodycode.internalDeepCopy(start,end)));

return copy;
}


}