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
public  class Module extends Syntax implements ProcMember {
 public  boolean isMarkedPure; 
 public  QualName name; 
public final List<ModuleFileMember> bits;
public Module(Token start, Token end, boolean isMarkedPure, QualName name, List<ModuleFileMember> bits){
    super(start, end);
    this.isMarkedPure = isMarkedPure;

    this.name = name;
    if(name != null) children.add(name);
    this.bits = bits;
for(Object o : bits){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Module)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Module)this, arg);
}







public String details(){
return "Module("
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    + " "
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "bits=" + "["+Syntax.sepDetails(this.bits, ",")+"]"
    +")";
}
public static String detstr(
Object isMarkedPure, Object name, Object bits
) {
return "Module(" + 
"isMarkedPure=" + isMarkedPure+" "+"name=" + name+" "+"bits=" + bits
+ ")";}
public String toString() {return 
     (isMarkedPure ? "pure " : "") + 
     "module " + name + "{" + sep(bits, "") + "} "
  ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.isMarkedPure; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isMarkedPure) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.bits.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : isMarkedPure
              // 1 : name
              if (name != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : bits
              for(int i = 0; i < this.bits.size(); i++) {                 ptrs.add(new SynPtr(this, 2, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.isMarkedPure = (Boolean) newval; break;
		 case 1: this.name = (QualName) newval; break;
		 case 2: this.bits.set(index, (ModuleFileMember) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Module";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Module(start, end, isMarkedPure, 
(name == null ? null : (QualName)name.internalDeepCopy(start,end)), (List<ModuleFileMember>)(deepCopyList(bits, start, end)));

return copy;
}


  public SealForModule moduleSeal;
  
}