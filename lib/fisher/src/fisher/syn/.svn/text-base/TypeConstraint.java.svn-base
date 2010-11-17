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
public  class TypeConstraint extends Syntax implements ProcMember {
 public  Id typename; 
public TypeConstraint(Token start, Token end, Id typename){
    super(start, end);
    this.typename = typename;
    if(typename != null) children.add(typename);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TypeConstraint)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((TypeConstraint)this, arg);
}







public String details(){
return "TypeConstraint("
    + "typename=" + Syntax.detailsome(this.typename)
    +")";
}
public static String detstr(
Object typename
) {
return "TypeConstraint(" + 
"typename=" + typename
+ ")";}
public String toString() {return typename.toString();}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.typename; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (typename) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : typename
              if (typename != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.typename = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "TypeConstraint";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new TypeConstraint(start, end, 
(typename == null ? null : (Id)typename.internalDeepCopy(start,end)));

return copy;
}


}