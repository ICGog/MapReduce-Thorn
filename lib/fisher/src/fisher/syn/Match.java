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
public  class Match extends Cmd implements ProcMember {
 public  Cmd subject; 
public final List<Case> cases;
public Match(Token start, Token end, Cmd subject, List<Case> cases){
    super(start, end);
    this.subject = subject;
    if(subject != null) children.add(subject);
    this.cases = cases;
for(Object o : cases){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Match)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Match)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Match)this, arg);
}






public String details(){
return "Match("
    + "subject=" + Syntax.detailsome(this.subject)
    + " "
    + "cases=" + "["+Syntax.sepDetails(this.cases, ",")+"]"
    +")";
}
public static String detstr(
Object subject, Object cases
) {
return "Match(" + 
"subject=" + subject+" "+"cases=" + cases
+ ")";}
public String toString() {return "match " + subject + "{" + sep(cases, " | ") + "} ";}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.subject; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (subject) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.cases.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : subject
              if (subject != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : cases
              for(int i = 0; i < this.cases.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.subject = (Cmd) newval; break;
		 case 1: this.cases.set(index, (Case) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Match";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Match(start, end, 
(subject == null ? null : (Cmd)subject.internalDeepCopy(start,end)), (List<Case>)(deepCopyList(cases, start, end)));

return copy;
}


}