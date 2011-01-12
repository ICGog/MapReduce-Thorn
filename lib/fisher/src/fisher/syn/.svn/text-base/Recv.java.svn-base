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
public  class Recv extends Cmd implements ProcMember {
public final List<Case> cases;
 public  Cmd timeoutLen; 
 public  Cmd timeoutCmd; 
public Recv(Token start, Token end, List<Case> cases, Cmd timeoutLen, Cmd timeoutCmd){
    super(start, end);
    this.cases = cases;
for(Object o : cases){children.add((Syntax)o);}
    this.timeoutLen = timeoutLen;
    if(timeoutLen != null) children.add(timeoutLen);
    this.timeoutCmd = timeoutCmd;
    if(timeoutCmd != null) children.add(timeoutCmd);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Recv)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Recv)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Recv)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Recv)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Recv)this, arg);
}
public String details(){
return "Recv("
    + "cases=" + "["+Syntax.sepDetails(this.cases, ",")+"]"
    + " "
    + "timeoutLen=" + Syntax.detailsome(this.timeoutLen)
    + " "
    + "timeoutCmd=" + Syntax.detailsome(this.timeoutCmd)
    +")";
}
public static String detstr(
Object cases, Object timeoutLen, Object timeoutCmd
) {
return "Recv(" + 
"cases=" + cases+" "+"timeoutLen=" + timeoutLen+" "+"timeoutCmd=" + timeoutCmd
+ ")";}
public String toString() {return  "recv{"  + sep(cases, " | ") + 
     (timeoutLen == null ? "" : " timeout(" + timeoutLen + ")" + timeoutCmd )+
     "} ";}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.cases.get(index); ; break;
                     case 1: ret = this.timeoutLen; ; break;
                     case 2: ret = this.timeoutCmd; ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : cases
              for(int i = 0; i < this.cases.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              // 1 : timeoutLen
              if (timeoutLen != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : timeoutCmd
              if (timeoutCmd != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.cases.set(index, (Case) newval); break;
		 case 1: this.timeoutLen = (Cmd) newval; break;
		 case 2: this.timeoutCmd = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Recv";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Recv(start, end, (List<Case>)(deepCopyList(cases, start, end)), 
(timeoutLen == null ? null : (Cmd)timeoutLen.internalDeepCopy(start,end)), 
(timeoutCmd == null ? null : (Cmd)timeoutCmd.internalDeepCopy(start,end)));

     try {
       fisher.ingest.Ingester.IT.visit((Recv)copy, null); // sort copied cases by prio
     }catch(fisher.util.FisherException fe) {
       fe.printStackTrace();
     }
   
return copy;
}


        public List< List<Case> > casesByPrio;
     
}