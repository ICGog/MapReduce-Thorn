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
public  class ModuleFileImport extends Syntax implements ModuleFileMember {
 public  ImportStmt imp; 
public ModuleFileImport(Token start, Token end, ImportStmt imp){
    super(start, end);
    this.imp = imp;
    if(imp != null) children.add(imp);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileImport)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileImport)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileImport)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileImport)this, arg);
}
public String details(){
return "ModuleFileImport("
    + "imp=" + Syntax.detailsome(this.imp)
    +")";
}
public static String detstr(
Object imp
) {
return "ModuleFileImport(" + 
"imp=" + imp
+ ")";}
public String toString() {return  imp.toString() ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.imp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (imp) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : imp
              if (imp != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.imp = (ImportStmt) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ModuleFileImport";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ModuleFileImport(start, end, 
(imp == null ? null : (ImportStmt)imp.internalDeepCopy(start,end)));

return copy;
}


}