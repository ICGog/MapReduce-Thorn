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
public  class ModuleFileVisibility extends Syntax implements ModuleFileMember {
 public  Visibility vis; 
 public  Id id; 
public ModuleFileVisibility(Token start, Token end, Visibility vis, Id id){
    super(start, end);
    this.vis = vis;

    this.id = id;
    if(id != null) children.add(id);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileVisibility)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileVisibility)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileVisibility)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileVisibility)this, arg);
}
public String details(){
return "ModuleFileVisibility("
    + "vis=" + Syntax.detailsome(this.vis)
    + " "
    + "id=" + Syntax.detailsome(this.id)
    +")";
}
public static String detstr(
Object vis, Object id
) {
return "ModuleFileVisibility(" + 
"vis=" + vis+" "+"id=" + id
+ ")";}
public String toString() {return  vis + " " + id ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.vis; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (vis) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : vis
              // 1 : id
              if (id != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.vis = (Visibility) newval; break;
		 case 1: this.id = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ModuleFileVisibility";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ModuleFileVisibility(start, end, vis, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)));

return copy;
}


}