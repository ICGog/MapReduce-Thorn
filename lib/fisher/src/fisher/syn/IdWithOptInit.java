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
public  class IdWithOptInit extends Syntax implements ProcMember {
 public  Id id; 
 public  Cmd init; 
 public  TypeConstraints typeConstraints; 
 public  ColAccess colAccess; 
public IdWithOptInit(Token start, Token end, Id id, Cmd init, TypeConstraints typeConstraints, ColAccess colAccess){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
    this.init = init;
    if(init != null) children.add(init);
    this.typeConstraints = typeConstraints;
    if(typeConstraints != null) children.add(typeConstraints);
    this.colAccess = colAccess;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((IdWithOptInit)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((IdWithOptInit)this, arg);
}







public String details(){
return "IdWithOptInit("
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "init=" + Syntax.detailsome(this.init)
    + " "
    + "typeConstraints=" + Syntax.detailsome(this.typeConstraints)
    + " "
    + "colAccess=" + Syntax.detailsome(this.colAccess)
    +")";
}
public static String detstr(
Object id, Object init, Object typeConstraints, Object colAccess
) {
return "IdWithOptInit(" + 
"id=" + id+" "+"init=" + init+" "+"typeConstraints=" + typeConstraints+" "+"colAccess=" + colAccess
+ ")";}
public String toString() {return 
     id 
     + typeConstraints.toString()
     + (init == null ? "" 
        : " " + colAccess.op + " " + init
       )
   ;}




 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.init; ; break;
                     case 2: ret = this.typeConstraints; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (typeConstraints) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.colAccess; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (colAccess) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : init
              if (init != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : typeConstraints
              if (typeConstraints != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : colAccess
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;
		 case 1: this.init = (Cmd) newval; break;
		 case 2: this.typeConstraints = (TypeConstraints) newval; break;
		 case 3: this.colAccess = (ColAccess) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "IdWithOptInit";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new IdWithOptInit(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), 
(init == null ? null : (Cmd)init.internalDeepCopy(start,end)), 
(typeConstraints == null ? null : (TypeConstraints)typeConstraints.internalDeepCopy(start,end)), colAccess);

return copy;
}


}