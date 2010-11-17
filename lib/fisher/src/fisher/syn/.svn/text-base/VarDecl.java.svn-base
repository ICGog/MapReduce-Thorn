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
public  class VarDecl extends Cmd implements ModuleFileMember, ClassMember, ObjectMember, ProcMember, LocalMember {
 public  Id var; 
 public  Cmd init; 
 public  TypeConstraints typeConstraints; 
public VarDecl(Token start, Token end, Id var, Cmd init, TypeConstraints typeConstraints){
    super(start, end);
    this.var = var;
    if(var != null) children.add(var);
    this.init = init;
    if(init != null) children.add(init);
    this.typeConstraints = typeConstraints;
    if(typeConstraints != null) children.add(typeConstraints);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((VarDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ClassMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClassMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((VarDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ObjectMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ObjectMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((VarDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((VarDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(LocalMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(LocalMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((VarDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((VarDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((VarDecl)this, arg);
}
public String details(){
return "VarDecl("
    + "var=" + Syntax.detailsome(this.var)
    + " "
    + "init=" + Syntax.detailsome(this.init)
    + " "
    + "typeConstraints=" + Syntax.detailsome(this.typeConstraints)
    +")";
}
public static String detstr(
Object var, Object init, Object typeConstraints
) {
return "VarDecl(" + 
"var=" + var+" "+"init=" + init+" "+"typeConstraints=" + typeConstraints
+ ")";}
public String toString() {return  "var " + var + 
           typeConstraints + 
           (init == null ? ";" : " := " + init + ";")
           ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.var; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (var) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.init; ; break;
                     case 2: ret = this.typeConstraints; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (typeConstraints) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : var
              if (var != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : init
              if (init != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : typeConstraints
              if (typeConstraints != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.var = (Id) newval; break;
		 case 1: this.init = (Cmd) newval; break;
		 case 2: this.typeConstraints = (TypeConstraints) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "VarDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new VarDecl(start, end, 
(var == null ? null : (Id)var.internalDeepCopy(start,end)), 
(init == null ? null : (Cmd)init.internalDeepCopy(start,end)), 
(typeConstraints == null ? null : (TypeConstraints)typeConstraints.internalDeepCopy(start,end)));

return copy;
}


}