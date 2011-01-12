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
public  class Table extends AbstractTable implements ProcMember {
public final List<TableKey> keys;
public final List<TableFields> vals;
public Table(Token start, Token end, List<TableKey> keys, List<TableFields> vals){
    super(start, end);
    this.keys = keys;
for(Object o : keys){children.add((Syntax)o);}
    this.vals = vals;
for(Object o : vals){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Table)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Table)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Table)this, arg);
}






public String details(){
return "Table("
    + "keys=" + "["+Syntax.sepDetails(this.keys, ",")+"]"
    + " "
    + "vals=" + "["+Syntax.sepDetails(this.vals, ",")+"]"
    +")";
}
public static String detstr(
Object keys, Object vals
) {
return "Table(" + 
"keys=" + keys+" "+"vals=" + vals
+ ")";}
public String toString() {return  
      "table(" + sep(keys, ",") + ")" 
      + 
      "{" + sep(vals, " ") + "} "
      ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.keys.get(index); ; break;
                     case 1: ret = this.vals.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : keys
              for(int i = 0; i < this.keys.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              // 1 : vals
              for(int i = 0; i < this.vals.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.keys.set(index, (TableKey) newval); break;
		 case 1: this.vals.set(index, (TableFields) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Table";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Table(start, end, (List<TableKey>)(deepCopyList(keys, start, end)), (List<TableFields>)(deepCopyList(vals, start, end)));

return copy;
}


}