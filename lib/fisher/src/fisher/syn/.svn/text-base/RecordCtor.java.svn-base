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
public  class RecordCtor extends Cmd implements ProcMember {
public final List<RecordField> fields;
public RecordCtor(Token start, Token end, List<RecordField> fields){
    super(start, end);
    this.fields = fields;
for(Object o : fields){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((RecordCtor)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((RecordCtor)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((RecordCtor)this, arg);
}






public String details(){
return "RecordCtor("
    + "fields=" + "["+Syntax.sepDetails(this.fields, ",")+"]"
    +")";
}
public static String detstr(
Object fields
) {
return "RecordCtor(" + 
"fields=" + fields
+ ")";}
public String toString() {return 
   "\u2039" + sep(fields, ", ") + "\u203A"
   ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.fields.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : fields
              for(int i = 0; i < this.fields.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.fields.set(index, (RecordField) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "RecordCtor";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new RecordCtor(start, end, (List<RecordField>)(deepCopyList(fields, start, end)));

return copy;
}

public boolean explike(){return true;}
}