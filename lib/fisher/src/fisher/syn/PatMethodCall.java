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
public  class PatMethodCall extends Pat implements ProcMember {
 public  Id methodName; 
 public  Pat subpat; 
public PatMethodCall(Token start, Token end, Id methodName, Pat subpat){
    super(start, end);
    this.methodName = methodName;
    if(methodName != null) children.add(methodName);
    this.subpat = subpat;
    if(subpat != null) children.add(subpat);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatMethodCall)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatMethodCall)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatMethodCall)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((PatMethodCall)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatMethodCall)this, arg);
}


public String details(){
return "PatMethodCall("
    + "methodName=" + Syntax.detailsome(this.methodName)
    + " "
    + "subpat=" + Syntax.detailsome(this.subpat)
    +")";
}
public static String detstr(
Object methodName, Object subpat
) {
return "PatMethodCall(" + 
"methodName=" + methodName+" "+"subpat=" + subpat
+ ")";}
public String toString() {return 
      "." + methodName + "("
      + (subpat == null ? "" : subpat.toString() )
      + ")"
    ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.methodName; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (methodName) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.subpat; ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : methodName
              if (methodName != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : subpat
              if (subpat != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.methodName = (Id) newval; break;
		 case 1: this.subpat = (Pat) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatMethodCall";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatMethodCall(start, end, 
(methodName == null ? null : (Id)methodName.internalDeepCopy(start,end)), 
(subpat == null ? null : (Pat)subpat.internalDeepCopy(start,end)));

return copy;
}

public boolean explike(){return true;}
}