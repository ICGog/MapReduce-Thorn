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
public  class Seq extends Cmd implements ProcMember {
public final List<Cmd> cmds;
public Seq(Token start, Token end, List<Cmd> cmds){
    super(start, end);
    this.cmds = cmds;
for(Object o : cmds){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Seq)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Seq)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Seq)this, arg);
}






public String details() {return "{" + sepDetails(cmds, " ") + "}";}
public static String detstr(
Object cmds
) {
return "Seq(" + 
"cmds=" + cmds
+ ")";}
public String toString() {return "{" + sepStmt(cmds, " ") + "} ";}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.cmds.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : cmds
              for(int i = 0; i < this.cmds.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.cmds.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Seq";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Seq(start, end, (List<Cmd>)(deepCopyList(cmds, start, end)));

return copy;
}


}