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
public  class ModulesInAList extends Syntax implements ProcMember {
public final List<Module> modules;
public ModulesInAList(Token start, Token end, List<Module> modules){
    super(start, end);
    this.modules = modules;
for(Object o : modules){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModulesInAList)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModulesInAList)this, arg);
}







public String details(){
return "ModulesInAList("
    + "modules=" + "["+Syntax.sepDetails(this.modules, ",")+"]"
    +")";
}
public static String detstr(
Object modules
) {
return "ModulesInAList(" + 
"modules=" + modules
+ ")";}
public String toString() {return sep(modules, "\n");}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.modules.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : modules
              for(int i = 0; i < this.modules.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.modules.set(index, (Module) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ModulesInAList";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ModulesInAList(start, end, (List<Module>)(deepCopyList(modules, start, end)));

return copy;
}

/*This is a convenience for testing.  Not to be actually used. unless useful!*/
}