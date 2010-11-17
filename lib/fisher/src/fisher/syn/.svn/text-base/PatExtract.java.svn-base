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
public  class PatExtract extends Pat implements ProcMember {
 public  QualName patname; 
public final List<Pat> subpats;
public PatExtract(Token start, Token end, QualName patname, List<Pat> subpats){
    super(start, end);
    this.patname = patname;
    if(patname != null) children.add(patname);
    this.subpats = subpats;
for(Object o : subpats){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatExtract)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatExtract)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatExtract)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((PatExtract)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatExtract)this, arg);
}


public String details(){
return "PatExtract("
    + "patname=" + Syntax.detailsome(this.patname)
    + " "
    + "subpats=" + "["+Syntax.sepDetails(this.subpats, ",")+"]"
    +")";
}
public static String detstr(
Object patname, Object subpats
) {
return "PatExtract(" + 
"patname=" + patname+" "+"subpats=" + subpats
+ ")";}
public String toString() {return  patname + "(" + 
      sep(subpats, ", ") + ")" ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.patname; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (patname) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.subpats.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : patname
              if (patname != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : subpats
              for(int i = 0; i < this.subpats.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.patname = (QualName) newval; break;
		 case 1: this.subpats.set(index, (Pat) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatExtract";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatExtract(start, end, 
(patname == null ? null : (QualName)patname.internalDeepCopy(start,end)), (List<Pat>)(deepCopyList(subpats, start, end)));

return copy;
}


}