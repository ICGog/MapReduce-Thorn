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
public  class SortKey extends Syntax implements ProcMember {
 public  SortOrder sortOrder; 
 public  Cmd key; 
public SortKey(Token start, Token end, SortOrder sortOrder, Cmd key){
    super(start, end);
    this.sortOrder = sortOrder;

    this.key = key;
    if(key != null) children.add(key);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SortKey)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((SortKey)this, arg);
}







public String details(){
return "SortKey("
    + "sortOrder=" + Syntax.detailsome(this.sortOrder)
    + " "
    + "key=" + Syntax.detailsome(this.key)
    +")";
}
public static String detstr(
Object sortOrder, Object key
) {
return "SortKey(" + 
"sortOrder=" + sortOrder+" "+"key=" + key
+ ")";}
public String toString() {return  sortOrder.op + " " + key ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.sortOrder; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (sortOrder) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.key; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (key) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : sortOrder
              // 1 : key
              if (key != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.sortOrder = (SortOrder) newval; break;
		 case 1: this.key = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "SortKey";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new SortKey(start, end, sortOrder, 
(key == null ? null : (Cmd)key.internalDeepCopy(start,end)));

return copy;
}


}