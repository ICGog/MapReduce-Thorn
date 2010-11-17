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
public  class JavalyNewDecl extends Javaly implements ProcMember {
 public  Id name; 
public final List<Id> formals;
public JavalyNewDecl(Token start, Token end, Id name, List<Id> formals){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.formals = formals;
for(Object o : formals){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyNewDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyNewDecl)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyNewDecl)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((JavalyNewDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((JavalyNewDecl)this, arg);
}
public String details(){
return "JavalyNewDecl("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "formals=" + "["+Syntax.sepDetails(this.formals, ",")+"]"
    +")";
}
public static String detstr(
Object name, Object formals
) {
return "JavalyNewDecl(" + 
"name=" + name+" "+"formals=" + formals
+ ")";}
public String toString() {return 
    "new " + name + "(" + sep(formals, ",") + ")" + ";"
  ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.formals.get(index); ; break;

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
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.formals.set(index, (Id) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "JavalyNewDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new JavalyNewDecl(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), (List<Id>)(deepCopyList(formals, start, end)));

return copy;
}


    public java.lang.reflect.Method method; 
  
}