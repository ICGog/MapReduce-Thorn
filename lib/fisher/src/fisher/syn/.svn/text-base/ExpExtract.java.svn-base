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
public  class ExpExtract extends ForPatternOnly implements ProcMember {
 public  QualName patname; 
public final List<Cmd> inputs;
public final List<Cmd> subpats;
public ExpExtract(Token start, Token end, QualName patname, List<Cmd> inputs, List<Cmd> subpats){
    super(start, end);
    this.patname = patname;
    if(patname != null) children.add(patname);
    this.inputs = inputs;
for(Object o : inputs){children.add((Syntax)o);}
    this.subpats = subpats;
for(Object o : subpats){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ExpExtract)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ExpExtract)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ExpExtract)this, arg);
}






public String details(){
return "ExpExtract("
    + "patname=" + Syntax.detailsome(this.patname)
    + " "
    + "inputs=" + "["+Syntax.sepDetails(this.inputs, ",")+"]"
    + " "
    + "subpats=" + "["+Syntax.sepDetails(this.subpats, ",")+"]"
    +")";
}
public static String detstr(
Object patname, Object inputs, Object subpats
) {
return "ExpExtract(" + 
"patname=" + patname+" "+"inputs=" + inputs+" "+"subpats=" + subpats
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
                     case 1: ret = this.inputs.get(index); ; break;
                     case 2: ret = this.subpats.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : patname
              if (patname != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : inputs
              for(int i = 0; i < this.inputs.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : subpats
              for(int i = 0; i < this.subpats.size(); i++) {                 ptrs.add(new SynPtr(this, 2, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.patname = (QualName) newval; break;
		 case 1: this.inputs.set(index, (Cmd) newval); break;
		 case 2: this.subpats.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ExpExtract";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ExpExtract(start, end, 
(patname == null ? null : (QualName)patname.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(inputs, start, end)), (List<Cmd>)(deepCopyList(subpats, start, end)));

return copy;
}


}