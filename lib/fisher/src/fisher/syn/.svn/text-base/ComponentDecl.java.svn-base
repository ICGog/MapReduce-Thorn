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
public  class ComponentDecl extends Cmd implements ModuleFileMember, ProcMember {
 public  Id name; 
 public  Formals formals; 
public final List<ProcMember> bits;
 public  Cmd actualCode; 
public ComponentDecl(Token start, Token end, Id name, Formals formals, List<ProcMember> bits, Cmd actualCode){
    super(start, end);
    this.name = name;

    this.formals = formals;

    this.bits = bits;

    this.actualCode = actualCode;
    if(actualCode != null) children.add(actualCode);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ComponentDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ComponentDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ComponentDecl)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ComponentDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ComponentDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ComponentDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ComponentDecl)this, arg);
}
public String details(){
return "ComponentDecl("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "formals=" + Syntax.detailsome(this.formals)
    + " "
    + "bits=" + "["+Syntax.sepDetails(this.bits, ",")+"]"
    +")";
}
public static String detstr(
Object name, Object formals, Object bits
) {
return "ComponentDecl(" + 
"name=" + name+" "+"formals=" + formals+" "+"bits=" + bits
+ ")";}
public String toString() {return  "component " +name  + formals 
    + "{" + sepStmt(bits, " ") + "} " ;}




 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.formals; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (formals) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.bits.get(index); ; break;
                     case 3: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              // 1 : formals
              // 2 : bits
              // 3 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 3, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.formals = (Formals) newval; break;
		 case 2: this.bits.set(index, (ProcMember) newval); break;
		 case 3: this.actualCode = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ComponentDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ComponentDecl(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), 
(formals == null ? null : (Formals)formals.internalDeepCopy(start,end)), (List<ProcMember>)(deepCopyList(bits, start, end)), 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)));

return copy;
}


  public Spawn asSpawn() {
    return new Spawn(this.start, this.end, this.name, this.bits);
  }
  
}