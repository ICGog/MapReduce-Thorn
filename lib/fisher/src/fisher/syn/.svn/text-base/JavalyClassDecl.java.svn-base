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
public  class JavalyClassDecl extends Javaly implements ModuleFileMember, ProcMember {
 public  Id name; 
 public  QualName impl; 
public final List<JavalyMethodDecl> methods;
public final List<JavalyNewDecl> ctors;
public final List<Id> fields;
public JavalyClassDecl(Token start, Token end, Id name, QualName impl, List<JavalyMethodDecl> methods, List<JavalyNewDecl> ctors, List<Id> fields){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.impl = impl;
    if(impl != null) children.add(impl);
    this.methods = methods;
for(Object o : methods){children.add((Syntax)o);}
    this.ctors = ctors;
for(Object o : ctors){children.add((Syntax)o);}
    this.fields = fields;
for(Object o : fields){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyClassDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyClassDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyClassDecl)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyClassDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyClassDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyClassDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyClassDecl)this, arg);
}
public String details(){
return "JavalyClassDecl("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "impl=" + Syntax.detailsome(this.impl)
    + " "
    + "methods=" + "["+Syntax.sepDetails(this.methods, ",")+"]"
    + " "
    + "ctors=" + "["+Syntax.sepDetails(this.ctors, ",")+"]"
    + " "
    + "fields=" + "["+Syntax.sepDetails(this.fields, ",")+"]"
    +")";
}
public static String detstr(
Object name, Object impl, Object methods, Object ctors, Object fields
) {
return "JavalyClassDecl(" + 
"name=" + name+" "+"impl=" + impl+" "+"methods=" + methods+" "+"ctors=" + ctors+" "+"fields=" + fields
+ ")";}
public String toString() {return 
   "javaly class " + name 
   + (fields == null ? "" : "(" + sep(fields, ", ") + ")")
   + " = " + impl + " {" 
   + sep(methods, " ") + sep(ctors, " ") 
   + "} "
   ;}





 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.impl; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (impl) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.methods.get(index); ; break;
                     case 3: ret = this.ctors.get(index); ; break;
                     case 4: ret = this.fields.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              if (name != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : impl
              if (impl != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : methods
              for(int i = 0; i < this.methods.size(); i++) {                 ptrs.add(new SynPtr(this, 2, i));}
              // 3 : ctors
              for(int i = 0; i < this.ctors.size(); i++) {                 ptrs.add(new SynPtr(this, 3, i));}
              // 4 : fields
              for(int i = 0; i < this.fields.size(); i++) {                 ptrs.add(new SynPtr(this, 4, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.impl = (QualName) newval; break;
		 case 2: this.methods.set(index, (JavalyMethodDecl) newval); break;
		 case 3: this.ctors.set(index, (JavalyNewDecl) newval); break;
		 case 4: this.fields.set(index, (Id) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "JavalyClassDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new JavalyClassDecl(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), 
(impl == null ? null : (QualName)impl.internalDeepCopy(start,end)), (List<JavalyMethodDecl>)(deepCopyList(methods, start, end)), (List<JavalyNewDecl>)(deepCopyList(ctors, start, end)), (List<Id>)(deepCopyList(fields, start, end)));

return copy;
}


   public Class cls;
 
}