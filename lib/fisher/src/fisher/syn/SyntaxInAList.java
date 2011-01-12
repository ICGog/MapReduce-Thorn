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
public  class SyntaxInAList extends Syntax implements ProcMember {
public final List<Syntax> syns;
public SyntaxInAList(Token start, Token end, List<Syntax> syns){
    super(start, end);
    this.syns = syns;
for(Object o : syns){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SyntaxInAList)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((SyntaxInAList)this, arg);
}







public String details(){
return "SyntaxInAList("
    + "syns=" + "["+Syntax.sepDetails(this.syns, ",")+"]"
    +")";
}
public static String detstr(
Object syns
) {
return "SyntaxInAList(" + 
"syns=" + syns
+ ")";}
public String toString() {return  sep(syns, "\n") ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.syns.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : syns
              for(int i = 0; i < this.syns.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.syns.set(index, (Syntax) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "SyntaxInAList";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new SyntaxInAList(start, end, (List<Syntax>)(deepCopyList(syns, start, end)));

return copy;
}


}