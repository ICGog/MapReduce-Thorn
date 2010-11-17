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
public  class ClsPatDef extends Syntax implements ClassMember, ObjectMember {
 public  Id id; 
public final List<Id> formals;
 public  Cmd body; 
public ClsPatDef(Token start, Token end, Id id, List<Id> formals, Cmd body){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
    this.formals = formals;
for(Object o : formals){children.add((Syntax)o);}
    this.body = body;
    if(body != null) children.add(body);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsPatDef)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsPatDef)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ClassMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsPatDef)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClassMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsPatDef)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ObjectMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsPatDef)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ObjectMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsPatDef)this, arg);
}
public String details(){
return "ClsPatDef("
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "formals=" + "["+Syntax.sepDetails(this.formals, ",")+"]"
    + " "
    + "body=" + Syntax.detailsome(this.body)
    +")";
}
public static String detstr(
Object id, Object formals, Object body
) {
return "ClsPatDef(" + 
"id=" + id+" "+"formals=" + formals+" "+"body=" + body
+ ")";}
public String toString() {return 
    "pat " + id + "(" + sep(formals, ",") + ")" 
    + " = " + body + ""
   ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.formals.get(index); ; break;
                     case 2: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : formals
              for(int i = 0; i < this.formals.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : body
              if (body != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;
		 case 1: this.formals.set(index, (Id) newval); break;
		 case 2: this.body = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ClsPatDef";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ClsPatDef(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), (List<Id>)(deepCopyList(formals, start, end)), 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)));

return copy;
}


}