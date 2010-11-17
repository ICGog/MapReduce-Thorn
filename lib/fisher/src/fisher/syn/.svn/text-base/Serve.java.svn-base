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
public  class Serve extends Cmd implements ProcMember {
 public  ServeBlock before; 
 public  ServeBlock after; 
 public  Cmd timeout; 
 public  Cmd timeoutCmd; 
public final List<Case> cases;
public Serve(Token start, Token end, ServeBlock before, ServeBlock after, Cmd timeout, Cmd timeoutCmd, List<Case> cases){
    super(start, end);
    this.before = before;

    this.after = after;

    this.timeout = timeout;

    this.timeoutCmd = timeoutCmd;

    this.cases = cases;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Serve)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Serve)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Serve)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Serve)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Serve)this, arg);
}
public String details(){
return "Serve("
    + "before=" + Syntax.detailsome(this.before)
    + " "
    + "after=" + Syntax.detailsome(this.after)
    + " "
    + "timeout=" + Syntax.detailsome(this.timeout)
    + " "
    + "timeoutCmd=" + Syntax.detailsome(this.timeoutCmd)
    + " "
    + "cases=" + "["+Syntax.sepDetails(this.cases, ",")+"]"
    +")";
}
public static String detstr(
Object before, Object after, Object timeout, Object timeoutCmd, Object cases
) {
return "Serve(" + 
"before=" + before+" "+"after=" + after+" "+"timeout=" + timeout+" "+"timeoutCmd=" + timeoutCmd+" "+"cases=" + cases
+ ")";}
public String toString() {return 
     "serve"
    + (before == null ? "" : " " + before + "")
    + (after == null ? "" : " " + after + "")
    + (timeout == null ? "" : " timeout(" + timeout + ")"
       + (timeoutCmd == null ? "" : timeoutCmd.toString())
       )
    + (cases.isEmpty() ? "" : " catch {" + sep(cases, " | ") + "} ")
    + ";"
   ;}





 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.before; ; break;
                     case 1: ret = this.after; ; break;
                     case 2: ret = this.timeout; ; break;
                     case 3: ret = this.timeoutCmd; ; break;
                     case 4: ret = this.cases.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : before
              // 1 : after
              // 2 : timeout
              // 3 : timeoutCmd
              // 4 : cases
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.before = (ServeBlock) newval; break;
		 case 1: this.after = (ServeBlock) newval; break;
		 case 2: this.timeout = (Cmd) newval; break;
		 case 3: this.timeoutCmd = (Cmd) newval; break;
		 case 4: this.cases.set(index, (Case) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Serve";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Serve(start, end, 
(before == null ? null : (ServeBlock)before.internalDeepCopy(start,end)), 
(after == null ? null : (ServeBlock)after.internalDeepCopy(start,end)), 
(timeout == null ? null : (Cmd)timeout.internalDeepCopy(start,end)), 
(timeoutCmd == null ? null : (Cmd)timeoutCmd.internalDeepCopy(start,end)), (List<Case>)(deepCopyList(cases, start, end)));

return copy;
}


      public ComponentInfo componentInfo;
      public Cmd actualCode;
   
}