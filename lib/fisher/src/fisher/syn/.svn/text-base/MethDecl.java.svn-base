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
public  class MethDecl extends Syntax implements ClassMember, ObjectMember, ProcMember, TableMember, ModuleFileMember, LocalMember, Puretic {
 public  Id name; 
 public  FunBody funbody; 
 public  boolean isMarkedPure; 
public MethDecl(Token start, Token end, Id name, FunBody funbody, boolean isMarkedPure){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.funbody = funbody;
    if(funbody != null) children.add(funbody);
    this.isMarkedPure = isMarkedPure;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ClassMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClassMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ObjectMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ObjectMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(TableMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(TableMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(LocalMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(LocalMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(PureticVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethDecl)this, arg);
}
public String details(){
return "MethDecl("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "funbody=" + Syntax.detailsome(this.funbody)
    + " "
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    +")";
}
public static String detstr(
Object name, Object funbody, Object isMarkedPure
) {
return "MethDecl(" + 
"name=" + name+" "+"funbody=" + funbody+" "+"isMarkedPure=" + isMarkedPure
+ ")";}
public String toString() {return  "def " + asStmt(funbody) ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.funbody; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (funbody) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.isMarkedPure; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isMarkedPure) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              if (name != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : funbody
              if (funbody != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : isMarkedPure
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.funbody = (FunBody) newval; break;
		 case 2: this.isMarkedPure = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "MethDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new MethDecl(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), 
(funbody == null ? null : (FunBody)funbody.internalDeepCopy(start,end)), isMarkedPure);

     ((MethDecl)copy).origin = origin;
   
return copy;
}

        public boolean isMarkedPure() {return this.isMarkedPure;}
     

     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     

      public Id idOfName(){return name;}
      public Syntax origin; 
    
}