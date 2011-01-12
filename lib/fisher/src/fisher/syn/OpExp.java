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
public  class OpExp extends Cmd implements ProcMember {
 public  Op op; 
public final List<Cmd> operands;
public OpExp(Token start, Token end, Op op, List<Cmd> operands){
    super(start, end);
    this.op = op;

    this.operands = operands;
for(Object o : operands){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((OpExp)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((OpExp)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((OpExp)this, arg);
}






public String details() {return       op.fixity.formatDetails(operands, op) ;}
public static String detstr(
Object op, Object operands
) {
return "OpExp(" + 
"op=" + op+" "+"operands=" + operands
+ ")";}
public String toString() {return 
     op.fixity.format(operands, op)
     ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.op; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (op) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.operands.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : op
              // 1 : operands
              for(int i = 0; i < this.operands.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.op = (Op) newval; break;
		 case 1: this.operands.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "OpExp";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new OpExp(start, end, op, (List<Cmd>)(deepCopyList(operands, start, end)));

return copy;
}

public boolean explike(){return true;}
}