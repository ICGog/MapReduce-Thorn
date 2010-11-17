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
public  class Probe extends Cmd implements ProcMember {
 public  Id id; 
public final List<Cmd> exps;
 public  int count; 
public Probe(Token start, Token end, Id id, List<Cmd> exps, int count){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
    this.exps = exps;
for(Object o : exps){children.add((Syntax)o);}
    this.count = count;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Probe)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Probe)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Probe)this, arg);
}






public String details(){
return "Probe("
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "exps=" + "["+Syntax.sepDetails(this.exps, ",")+"]"
    + " "
    + "count=" + Syntax.detailsome(this.count)
    +")";
}
public static String detstr(
Object id, Object exps, Object count
) {
return "Probe(" + 
"id=" + id+" "+"exps=" + exps+" "+"count=" + count
+ ")";}
public String toString() {return "~!@" + 
     (id == null ? "" : id) 
     + "(" + sep(exps, ", ") + ")"
     + (count >= 0 ? "@!~" + count : "")
     ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; ; break;
                     case 1: ret = this.exps.get(index); ; break;
                     case 2: ret = this.count; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (count) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : exps
              for(int i = 0; i < this.exps.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : count
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;
		 case 1: this.exps.set(index, (Cmd) newval); break;
		 case 2: this.count = (Integer) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Probe";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Probe(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(exps, start, end)), count);
 ((Probe)copy).prober = this.prober; 
return copy;
}

 public fisher.test.Prober prober; 
 public Object scratch;
public boolean explike(){return true;}
}