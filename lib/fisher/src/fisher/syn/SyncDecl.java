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
public  class SyncDecl extends HighLevelCommunication implements ProcMember {

public SyncDecl(Token start, Token end, Id name, FunBody funbody){
    super(start, end, name, funbody);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SyncDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((SyncDecl)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SyncDecl)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((SyncDecl)this, arg);
}
public String details(){
return "SyncDecl("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "funbody=" + Syntax.detailsome(this.funbody)
    +")";
}
public static String detstr(
Object name, Object funbody
) {
return "SyncDecl(" + 
"name=" + name+" "+"funbody=" + funbody
+ ")";}
public String toString() {return  "sync " + (funbody) ;}

 
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
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.funbody = (FunBody) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "SyncDecl";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new SyncDecl(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), 
(funbody == null ? null : (FunBody)funbody.internalDeepCopy(start,end)));

return copy;
}


}