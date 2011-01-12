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
public  class ImportStmt extends Cmd implements ClassMember, ObjectMember, ProcMember, TableMember, ModuleFileMember, LocalMember {
 public  boolean isOwn; 
 public  QualName modName; 
public final List<ModArgBinding> modArgs;
 public  Id as; 
 public  boolean dotstar; 
public ImportStmt(Token start, Token end, boolean isOwn, QualName modName, List<ModArgBinding> modArgs, Id as, boolean dotstar){
    super(start, end);
    this.isOwn = isOwn;

    this.modName = modName;
    if(modName != null) children.add(modName);
    this.modArgs = modArgs;
for(Object o : modArgs){children.add((Syntax)o);}
    this.as = as;
    if(as != null) children.add(as);
    this.dotstar = dotstar;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ClassMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClassMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ObjectMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ObjectMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(TableMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(TableMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(LocalMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(LocalMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ImportStmt)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ImportStmt)this, arg);
}
public String details(){
return "ImportStmt("
    + "isOwn=" + Syntax.detailsome(this.isOwn)
    + " "
    + "modName=" + Syntax.detailsome(this.modName)
    + " "
    + "modArgs=" + "["+Syntax.sepDetails(this.modArgs, ",")+"]"
    + " "
    + "as=" + Syntax.detailsome(this.as)
    + " "
    + "dotstar=" + Syntax.detailsome(this.dotstar)
    +")";
}
public static String detstr(
Object isOwn, Object modName, Object modArgs, Object as, Object dotstar
) {
return "ImportStmt(" + 
"isOwn=" + isOwn+" "+"modName=" + modName+" "+"modArgs=" + modArgs+" "+"as=" + as+" "+"dotstar=" + dotstar
+ ")";}
public String toString() {return 
      "import "
      + (isOwn ? " own " : "")  
      + (as == null ? "" : as + " = ")
      + modName
      + (dotstar ? ".*" : "")
      + (modArgs.isEmpty() ? "" : " (" + sep(modArgs, ", ") + ")")
      + ""
      + ";"
    ;}





 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.isOwn; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isOwn) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.modName; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (modName) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.modArgs.get(index); ; break;
                     case 3: ret = this.as; ; break;
                     case 4: ret = this.dotstar; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (dotstar) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : isOwn
              // 1 : modName
              if (modName != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : modArgs
              for(int i = 0; i < this.modArgs.size(); i++) {                 ptrs.add(new SynPtr(this, 2, i));}
              // 3 : as
              if (as != null) ptrs.add(new SynPtr(this, 3, -1)); 
              // 4 : dotstar
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.isOwn = (Boolean) newval; break;
		 case 1: this.modName = (QualName) newval; break;
		 case 2: this.modArgs.set(index, (ModArgBinding) newval); break;
		 case 3: this.as = (Id) newval; break;
		 case 4: this.dotstar = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ImportStmt";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ImportStmt(start, end, isOwn, 
(modName == null ? null : (QualName)modName.internalDeepCopy(start,end)), (List<ModArgBinding>)(deepCopyList(modArgs, start, end)), 
(as == null ? null : (Id)as.internalDeepCopy(start,end)), dotstar);

return copy;
}


    public fisher.syn.importage.AbstractImport importage; 
    
}