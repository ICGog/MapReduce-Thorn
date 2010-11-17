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
public  class TypedExp extends Cmd implements ProcMember {
 public  Cmd exp; 
 public  QualName type; 
public TypedExp(Token start, Token end, Cmd exp, QualName type){
    super(start, end);
    this.exp = exp;
    if(exp != null) children.add(exp);
    this.type = type;
    if(type != null) children.add(type);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TypedExp)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((TypedExp)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TypedExp)this, arg);
}






public String details(){
return "TypedExp("
    + "exp=" + Syntax.detailsome(this.exp)
    + " "
    + "type=" + Syntax.detailsome(this.type)
    +")";
}
public static String detstr(
Object exp, Object type
) {
return "TypedExp(" + 
"exp=" + exp+" "+"type=" + type
+ ")";}
public String toString() {return  exp + ":" + type ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.exp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (exp) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.type; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (type) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : exp
              if (exp != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : type
              if (type != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.exp = (Cmd) newval; break;
		 case 1: this.type = (QualName) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "TypedExp";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new TypedExp(start, end, 
(exp == null ? null : (Cmd)exp.internalDeepCopy(start,end)), 
(type == null ? null : (QualName)type.internalDeepCopy(start,end)));

return copy;
}


}