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
public  class PatMatchSomethingElse extends Pat implements ProcMember {
 public  Cmd exp; 
 public  Pat pat; 
public PatMatchSomethingElse(Token start, Token end, Cmd exp, Pat pat){
    super(start, end);
    this.exp = exp;
    if(exp != null) children.add(exp);
    this.pat = pat;
    if(pat != null) children.add(pat);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatMatchSomethingElse)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatMatchSomethingElse)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatMatchSomethingElse)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((PatMatchSomethingElse)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatMatchSomethingElse)this, arg);
}


public String details(){
return "PatMatchSomethingElse("
    + "exp=" + Syntax.detailsome(this.exp)
    + " "
    + "pat=" + Syntax.detailsome(this.pat)
    +")";
}
public static String detstr(
Object exp, Object pat
) {
return "PatMatchSomethingElse(" + 
"exp=" + exp+" "+"pat=" + pat
+ ")";}
public String toString() {return  exp + " ~ " + pat ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.exp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (exp) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : exp
              if (exp != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : pat
              if (pat != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.exp = (Cmd) newval; break;
		 case 1: this.pat = (Pat) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatMatchSomethingElse";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatMatchSomethingElse(start, end, 
(exp == null ? null : (Cmd)exp.internalDeepCopy(start,end)), 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)));

return copy;
}


}