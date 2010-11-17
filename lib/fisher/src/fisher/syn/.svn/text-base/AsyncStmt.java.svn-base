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
public  class AsyncStmt extends Cmd implements ProcMember {
 public  Cmd receiver; 
 public  Id id; 
public final List<Cmd> args;
 public  Cmd security; 
 public  Cmd actualCode; 
public AsyncStmt(Token start, Token end, Cmd receiver, Id id, List<Cmd> args, Cmd security, Cmd actualCode){
    super(start, end);
    this.receiver = receiver;

    this.id = id;

    this.args = args;

    this.security = security;
    if(security != null) children.add(security);
    this.actualCode = actualCode;
    if(actualCode != null) children.add(actualCode);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AsyncStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AsyncStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AsyncStmt)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AsyncStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AsyncStmt)this, arg);
}
public String details(){
return "AsyncStmt("
    + "receiver=" + Syntax.detailsome(this.receiver)
    + " "
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "args=" + "["+Syntax.sepDetails(this.args, ",")+"]"
    + " "
    + "security=" + Syntax.detailsome(this.security)
    +")";
}
public static String detstr(
Object receiver, Object id, Object args, Object security
) {
return "AsyncStmt(" + 
"receiver=" + receiver+" "+"id=" + id+" "+"args=" + args+" "+"security=" + security
+ ")";}
public String toString() {return  receiver + " <-- " + id + "(" + sep(args, ", ") + ")"
      + (security == null ? "" : " security " + security)
  ;}





 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.receiver; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (receiver) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.args.get(index); ; break;
                     case 3: ret = this.security; ; break;
                     case 4: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : receiver
              // 1 : id
              // 2 : args
              // 3 : security
              if (security != null) ptrs.add(new SynPtr(this, 3, -1)); 
              // 4 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 4, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.receiver = (Cmd) newval; break;
		 case 1: this.id = (Id) newval; break;
		 case 2: this.args.set(index, (Cmd) newval); break;
		 case 3: this.security = (Cmd) newval; break;
		 case 4: this.actualCode = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "AsyncStmt";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new AsyncStmt(start, end, 
(receiver == null ? null : (Cmd)receiver.internalDeepCopy(start,end)), 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(args, start, end)), 
(security == null ? null : (Cmd)security.internalDeepCopy(start,end)), 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)));

return copy;
}

public boolean explike(){return true;}
}