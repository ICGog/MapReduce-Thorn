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
public  class ModuleFileMemberStmt extends Syntax implements ModuleFileMember {
 public  String filename; 
public ModuleFileMemberStmt(Token start, Token end, String filename){
    super(start, end);
    this.filename = filename;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileMemberStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileMemberStmt)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileMemberStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileMemberStmt)this, arg);
}
public String details(){
return "ModuleFileMemberStmt("
    + "filename=" + Syntax.detailsome(this.filename)
    +")";
}
public static String detstr(
Object filename
) {
return "ModuleFileMemberStmt(" + 
"filename=" + filename
+ ")";}
public String toString() {return  "member \"" + filename + "\";" ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.filename; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (filename) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : filename
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.filename = (String) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ModuleFileMemberStmt";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ModuleFileMemberStmt(start, end, filename);

return copy;
}


}