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
public  class QueryTable extends QueryAbstract implements ProcMember {
public final List<IdWithOptInit> keys;
public final List<TableFields> items;
public QueryTable(Token start, Token end, List<QueryControl> controls, Cmd actualCode, List<IdWithOptInit> keys, List<TableFields> items){
    super(start, end, controls, actualCode);
    this.keys = keys;

    this.items = items;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryTable)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryTable)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryTable)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryAbstract<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryTable)this, arg);
}
public String details(){
return "QueryTable("
    + "controls=" + "["+Syntax.sepDetails(this.controls, ",")+"]"
    + " "
    + "keys=" + "["+Syntax.sepDetails(this.keys, ",")+"]"
    + " "
    + "items=" + "["+Syntax.sepDetails(this.items, ",")+"]"
    +")";
}
public static String detstr(
Object controls, Object keys, Object items
) {
return "QueryTable(" + 
"controls=" + controls+" "+"keys=" + keys+" "+"items=" + items
+ ")";}
public String toString() {return  
      "%table(" 
      + sep(keys, ", ")
      + ")"
      + "{"
      + sep(items, " ")
      + super.toString()
      + "} "
    ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.controls.get(index); ; break;
                     case 1: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.keys.get(index); ; break;
                     case 3: ret = this.items.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : controls
              // 1 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : keys
              // 3 : items
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.controls.set(index, (QueryControl) newval); break;
		 case 1: this.actualCode = (Cmd) newval; break;
		 case 2: this.keys.set(index, (IdWithOptInit) newval); break;
		 case 3: this.items.set(index, (TableFields) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryTable";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryTable(start, end, (List<QueryControl>)(deepCopyList(controls, start, end)), 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)), (List<IdWithOptInit>)(deepCopyList(keys, start, end)), (List<TableFields>)(deepCopyList(items, start, end)));

return copy;
}

public boolean explike(){return true;}
}