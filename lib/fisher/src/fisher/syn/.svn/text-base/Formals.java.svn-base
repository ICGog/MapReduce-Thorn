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
public  class Formals extends Syntax implements ProcMember {
public final List<Pat> formals;
public Formals(Token start, Token end, List<Pat> formals){
    super(start, end);
    this.formals = formals;
for(Object o : formals){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Formals)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Formals)this, arg);
}







public String details() {return "(" + sepDetails(formals, ",") + ")"  ;}
public static String detstr(
Object formals
) {
return "Formals(" + 
"formals=" + formals
+ ")";}
public String toString() {return "(" + sep(formals, ",") + ")" ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.formals.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : formals
              for(int i = 0; i < this.formals.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.formals.set(index, (Pat) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Formals";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Formals(start, end, (List<Pat>)(deepCopyList(formals, start, end)));

return copy;
}


}