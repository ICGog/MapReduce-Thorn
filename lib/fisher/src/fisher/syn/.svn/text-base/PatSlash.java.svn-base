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
public  class PatSlash extends Pat implements ProcMember {
 public  Cmd re; 
 public  Pat pat; 
 public  Pat actualCode; 
public PatSlash(Token start, Token end, Cmd re, Pat pat, Pat actualCode){
    super(start, end);
    this.re = re;

    this.pat = pat;

    this.actualCode = actualCode;
    if(actualCode != null) children.add(actualCode);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatSlash)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatSlash)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatSlash)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((PatSlash)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatSlash)this, arg);
}


public String details(){
return "PatSlash("
    + "re=" + Syntax.detailsome(this.re)
    + " "
    + "pat=" + Syntax.detailsome(this.pat)
    +")";
}
public static String detstr(
Object re, Object pat
) {
return "PatSlash(" + 
"re=" + re+" "+"pat=" + pat
+ ")";}
public String toString() {return 
     re + "/" + pat
   ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.re; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (re) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : re
              // 1 : pat
              // 2 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.re = (Cmd) newval; break;
		 case 1: this.pat = (Pat) newval; break;
		 case 2: this.actualCode = (Pat) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatSlash";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatSlash(start, end, 
(re == null ? null : (Cmd)re.internalDeepCopy(start,end)), 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)), 
(actualCode == null ? null : (Pat)actualCode.internalDeepCopy(start,end)));

return copy;
}


}