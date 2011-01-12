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
public  class TableKey extends Syntax implements ProcMember {
 public  Id name; 
 public  TypeConstraints typeConstraints; 
public TableKey(Token start, Token end, Id name, TypeConstraints typeConstraints){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.typeConstraints = typeConstraints;
    if(typeConstraints != null) children.add(typeConstraints);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TableKey)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((TableKey)this, arg);
}







public String details(){
return "TableKey("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "typeConstraints=" + Syntax.detailsome(this.typeConstraints)
    +")";
}
public static String detstr(
Object name, Object typeConstraints
) {
return "TableKey(" + 
"name=" + name+" "+"typeConstraints=" + typeConstraints
+ ")";}
public String toString() {return name + "" + typeConstraints;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.typeConstraints; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (typeConstraints) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              if (name != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : typeConstraints
              if (typeConstraints != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.typeConstraints = (TypeConstraints) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "TableKey";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new TableKey(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), 
(typeConstraints == null ? null : (TypeConstraints)typeConstraints.internalDeepCopy(start,end)));

return copy;
}


}