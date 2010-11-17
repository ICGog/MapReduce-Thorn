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
public  class Send extends Cmd implements ProcMember {
 public  Cmd receiver; 
 public  Cmd exp; 
 public  Cmd security; 
public Send(Token start, Token end, Cmd receiver, Cmd exp, Cmd security){
    super(start, end);
    this.receiver = receiver;
    if(receiver != null) children.add(receiver);
    this.exp = exp;
    if(exp != null) children.add(exp);
    this.security = security;
    if(security != null) children.add(security);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Send)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Send)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Send)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Send)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Send)this, arg);
}
public String details(){
return "Send("
    + "receiver=" + Syntax.detailsome(this.receiver)
    + " "
    + "exp=" + Syntax.detailsome(this.exp)
    + " "
    + "security=" + Syntax.detailsome(this.security)
    +")";
}
public static String detstr(
Object receiver, Object exp, Object security
) {
return "Send(" + 
"receiver=" + receiver+" "+"exp=" + exp+" "+"security=" + security
+ ")";}
public String toString() {return  receiver + " <<< " + exp
      + (security == null ? "" : " security " + security)
    ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.receiver; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (receiver) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.exp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (exp) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.security; ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : receiver
              if (receiver != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : exp
              if (exp != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : security
              if (security != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.receiver = (Cmd) newval; break;
		 case 1: this.exp = (Cmd) newval; break;
		 case 2: this.security = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Send";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Send(start, end, 
(receiver == null ? null : (Cmd)receiver.internalDeepCopy(start,end)), 
(exp == null ? null : (Cmd)exp.internalDeepCopy(start,end)), 
(security == null ? null : (Cmd)security.internalDeepCopy(start,end)));

return copy;
}

public boolean explike(){return true;}
}