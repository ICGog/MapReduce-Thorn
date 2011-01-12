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
public  class Valof extends Cmd implements ProcMember {
public final List<Cmd> stmts;
 public  boolean innerFrame; 
public Valof(Token start, Token end, List<Cmd> stmts, boolean innerFrame){
    super(start, end);
    this.stmts = stmts;
for(Object o : stmts){children.add((Syntax)o);}
    this.innerFrame = innerFrame;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Valof)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Valof)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Valof)this, arg);
}






public String details(){
return "Valof("
    + "stmts=" + "["+Syntax.sepDetails(this.stmts, ",")+"]"
    + " "
    + "innerFrame=" + Syntax.detailsome(this.innerFrame)
    +")";
}
public static String detstr(
Object stmts, Object innerFrame
) {
return "Valof(" + 
"stmts=" + stmts+" "+"innerFrame=" + innerFrame
+ ")";}
public String toString() {return  
       (innerFrame ? "valofInner{" : "valof{")
       + sepStmt(stmts, " ") + "} " ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.stmts.get(index); ; break;
                     case 1: ret = this.innerFrame; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (innerFrame) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : stmts
              for(int i = 0; i < this.stmts.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              // 1 : innerFrame
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.stmts.set(index, (Cmd) newval); break;
		 case 1: this.innerFrame = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Valof";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Valof(start, end, (List<Cmd>)(deepCopyList(stmts, start, end)), innerFrame);

return copy;
}

public boolean explike(){return true;}
}