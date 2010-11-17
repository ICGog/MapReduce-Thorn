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
public  class ClsDecl extends Cmd implements ModuleFileMember, Classlike, Puretic, ProcMember, LocalMember {
 public  boolean isMarkedPure; 
 public  Id name; 
 public  boolean hasParams; 
public final List<ClassFormal> params;
public final List<ClsExtends> exts;
public final List<ClassMember> members;
public ClsDecl(Token start, Token end, boolean isMarkedPure, Id name, boolean hasParams, List<ClassFormal> params, List<ClsExtends> exts, List<ClassMember> members){
    super(start, end);
    this.isMarkedPure = isMarkedPure;

    this.name = name;
    if(name != null) children.add(name);
    this.hasParams = hasParams;

    this.params = params;
for(Object o : params){children.add((Syntax)o);}
    this.exts = exts;
for(Object o : exts){children.add((Syntax)o);}
    this.members = members;
for(Object o : members){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(LocalMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(LocalMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ClasslikeVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClasslikeWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(PureticVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsDecl)this, arg);
}
public String details(){
return "ClsDecl("
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    + " "
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "hasParams=" + Syntax.detailsome(this.hasParams)
    + " "
    + "params=" + "["+Syntax.sepDetails(this.params, ",")+"]"
    + " "
    + "exts=" + "["+Syntax.sepDetails(this.exts, ",")+"]"
    + " "
    + "members=" + "["+Syntax.sepDetails(this.members, ",")+"]"
    +")";
}
public static String detstr(
Object isMarkedPure, Object name, Object hasParams, Object params, Object exts, Object members
) {
return "ClsDecl(" + 
"isMarkedPure=" + isMarkedPure+" "+"name=" + name+" "+"hasParams=" + hasParams+" "+"params=" + params+" "+"exts=" + exts+" "+"members=" + members
+ ")";}
public String toString() {return  
      ""
      + "class " + name 
      + (params.isEmpty() ? "" : "(" + sep(params, ", ") + ")"  )
      + (isMarkedPure ? ":pure" : "") 
      + (exts.isEmpty() ? "" : " extends " + sep(exts, ","))
      + "{"
      + sepStmt(members, " ")
      + "} "     
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
                     case 2: ret = this.hasParams; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (hasParams) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.params.get(index); ; break;
                     case 4: ret = this.exts.get(index); ; break;
                     case 5: ret = this.members.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : isMarkedPure
              // 1 : name
              if (name != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : hasParams
              // 3 : params
              for(int i = 0; i < this.params.size(); i++) {                 ptrs.add(new SynPtr(this, 3, i));}
              // 4 : exts
              for(int i = 0; i < this.exts.size(); i++) {                 ptrs.add(new SynPtr(this, 4, i));}
              // 5 : members
              for(int i = 0; i < this.members.size(); i++) {                 ptrs.add(new SynPtr(this, 5, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.isMarkedPure = (Boolean) newval; break;
		 case 1: this.name = (Id) newval; break;
		 case 2: this.hasParams = (Boolean) newval; break;
		 case 3: this.params.set(index, (ClassFormal) newval); break;
		 case 4: this.exts.set(index, (ClsExtends) newval); break;
		 case 5: this.members.set(index, (ClassMember) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ClsDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ClsDecl(start, end, isMarkedPure, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), hasParams, (List<ClassFormal>)(deepCopyList(params, start, end)), (List<ClsExtends>)(deepCopyList(exts, start, end)), (List<ClassMember>)(deepCopyList(members, start, end)));

return copy;
}

        public boolean isMarkedPure() {return this.isMarkedPure;}
     

     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     

       public ClassStatic classStatic;
       public SealForClass classSeal;
       public List<ClassFormal> params() {return this.params;}
       public Id name(){return this.name;};
       public boolean hasParams() {return this.hasParams;}
       public List<ClassMember> members() {return this.members;};
       public void setClassStatic(ClassStatic cs) {this.classStatic = cs;}
       public List<ClsExtends> extendses() {return this.exts;}
	
       public ClassStatic classStatic() {return this.classStatic;}
       public void setClassSeal(SealForClass classSeal) {this.classSeal = classSeal;}
       public SealForClass classSeal() {return this.classSeal;}
       public Id idOfName(){return this.name;}
        
}