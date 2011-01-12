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
public  class JavalyMethodDecl extends Javaly implements ProcMember {
 public  Id name; 
public final List<Id> formals;
 public  Id impl; 
public JavalyMethodDecl(Token start, Token end, Id name, List<Id> formals, Id impl){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.formals = formals;
for(Object o : formals){children.add((Syntax)o);}
    this.impl = impl;
    if(impl != null) children.add(impl);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyMethodDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyMethodDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyMethodDecl)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyMethodDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyMethodDecl)this, arg);
}
public String details(){
return "JavalyMethodDecl("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "formals=" + "["+Syntax.sepDetails(this.formals, ",")+"]"
    + " "
    + "impl=" + Syntax.detailsome(this.impl)
    +")";
}
public static String detstr(
Object name, Object formals, Object impl
) {
return "JavalyMethodDecl(" + 
"name=" + name+" "+"formals=" + formals+" "+"impl=" + impl
+ ")";}
public String toString() {return 
    "method " + name + "(" + sep(formals, ",") + ") = " + impl + ";"
  ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.formals.get(index); ; break;
                     case 2: ret = this.impl; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (impl) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              if (name != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : formals
              for(int i = 0; i < this.formals.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : impl
              if (impl != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.formals.set(index, (Id) newval); break;
		 case 2: this.impl = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "JavalyMethodDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new JavalyMethodDecl(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), (List<Id>)(deepCopyList(formals, start, end)), 
(impl == null ? null : (Id)impl.internalDeepCopy(start,end)));

return copy;
}


    public java.lang.reflect.Method method; 
  
}