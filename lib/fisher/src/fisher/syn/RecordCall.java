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
public  class RecordCall extends Cmd implements ProcMember {
 public  Cmd function; 
public final List<RecordField> fields;
public RecordCall(Token start, Token end, Cmd function, List<RecordField> fields){
    super(start, end);
    this.function = function;
    if(function != null) children.add(function);
    this.fields = fields;
for(Object o : fields){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((RecordCall)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((RecordCall)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((RecordCall)this, arg);
}






public String details(){
return "RecordCall("
    + "function=" + Syntax.detailsome(this.function)
    + " "
    + "fields=" + "["+Syntax.sepDetails(this.fields, ",")+"]"
    +")";
}
public static String detstr(
Object function, Object fields
) {
return "RecordCall(" + 
"function=" + function+" "+"fields=" + fields
+ ")";}
public String toString() {return  function +  "\u2039" + sep(fields, ", ") + "\u203A" ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.function; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (function) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.fields.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : function
              if (function != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : fields
              for(int i = 0; i < this.fields.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.function = (Cmd) newval; break;
		 case 1: this.fields.set(index, (RecordField) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "RecordCall";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new RecordCall(start, end, 
(function == null ? null : (Cmd)function.internalDeepCopy(start,end)), (List<RecordField>)(deepCopyList(fields, start, end)));

return copy;
}

public boolean explike(){return true;}
}