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
public  class ClsCtorDef extends Syntax implements ClassMember {
 public  Id id; 
 public  MonoBody monobody; 
public ClsCtorDef(Token start, Token end, Id id, MonoBody monobody){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
    this.monobody = monobody;
    if(monobody != null) children.add(monobody);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsCtorDef)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsCtorDef)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ClassMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsCtorDef)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClassMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsCtorDef)this, arg);
}
public String details(){
return "ClsCtorDef("
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "monobody=" + Syntax.detailsome(this.monobody)
    +")";
}
public static String detstr(
Object id, Object monobody
) {
return "ClsCtorDef(" + 
"id=" + id+" "+"monobody=" + monobody
+ ")";}
public String toString() {return  "new " + monobody ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.monobody; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (monobody) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : monobody
              if (monobody != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;
		 case 1: this.monobody = (MonoBody) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ClsCtorDef";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ClsCtorDef(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), 
(monobody == null ? null : (MonoBody)monobody.internalDeepCopy(start,end)));

return copy;
}


}