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
public  class FieldRef extends Cmd implements ProcMember {
 public  Cmd target; 
 public  Id field; 
public FieldRef(Token start, Token end, Cmd target, Id field){
    super(start, end);
    this.target = target;
    if(target != null) children.add(target);
    this.field = field;
    if(field != null) children.add(field);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((FieldRef)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((FieldRef)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((FieldRef)this, arg);
}






public String details(){
return "FieldRef("
    + "target=" + Syntax.detailsome(this.target)
    + " "
    + "field=" + Syntax.detailsome(this.field)
    +")";
}
public static String detstr(
Object target, Object field
) {
return "FieldRef(" + 
"target=" + target+" "+"field=" + field
+ ")";}
public String toString() {return target + "." + field;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.target; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (target) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.field; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (field) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : target
              if (target != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : field
              if (field != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.target = (Cmd) newval; break;
		 case 1: this.field = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "FieldRef";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new FieldRef(start, end, 
(target == null ? null : (Cmd)target.internalDeepCopy(start,end)), 
(field == null ? null : (Id)field.internalDeepCopy(start,end)));

return copy;
}

public boolean explike(){return true;}
}