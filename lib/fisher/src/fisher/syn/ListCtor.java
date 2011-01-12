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
public  class ListCtor extends Cmd implements ProcMember {
public final List<ListBit> bits;
public ListCtor(Token start, Token end, List<ListBit> bits){
    super(start, end);
    this.bits = bits;
for(Object o : bits){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ListCtor)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ListCtor)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ListCtor)this, arg);
}






public String details(){
return "ListCtor("
    + "bits=" + "["+Syntax.sepDetails(this.bits, ",")+"]"
    +")";
}
public static String detstr(
Object bits
) {
return "ListCtor(" + 
"bits=" + bits
+ ")";}
public String toString() {return  "[" + sep(bits, ", ") + "]";}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.bits.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : bits
              for(int i = 0; i < this.bits.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.bits.set(index, (ListBit) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ListCtor";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ListCtor(start, end, (List<ListBit>)(deepCopyList(bits, start, end)));

return copy;
}

public boolean explike(){return true;}
}