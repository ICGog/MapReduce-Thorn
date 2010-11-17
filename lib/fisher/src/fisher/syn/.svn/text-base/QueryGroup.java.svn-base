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
public  class QueryGroup extends QueryAbstract implements ProcMember {
public final List<QGKey> keys;
public final List<QGAccum> accums;
public QueryGroup(Token start, Token end, List<QueryControl> controls, Cmd actualCode, List<QGKey> keys, List<QGAccum> accums){
    super(start, end, controls, actualCode);
    this.keys = keys;

    this.accums = accums;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryGroup)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryGroup)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryGroup)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryAbstract<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryGroup)this, arg);
}
public String details(){
return "QueryGroup("
    + "controls=" + "["+Syntax.sepDetails(this.controls, ",")+"]"
    + " "
    + "keys=" + "["+Syntax.sepDetails(this.keys, ",")+"]"
    + " "
    + "accums=" + "["+Syntax.sepDetails(this.accums, ",")+"]"
    +")";
}
public static String detstr(
Object controls, Object keys, Object accums
) {
return "QueryGroup(" + 
"controls=" + controls+" "+"keys=" + keys+" "+"accums=" + accums
+ ")";}
public String toString() {return  
      "%group(" 
      + sep(keys, ", ")
      + ")"
      + "{"
      + sep(accums, " ")
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
                     case 3: ret = this.accums.get(index); ; break;

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
              // 3 : accums
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.controls.set(index, (QueryControl) newval); break;
		 case 1: this.actualCode = (Cmd) newval; break;
		 case 2: this.keys.set(index, (QGKey) newval); break;
		 case 3: this.accums.set(index, (QGAccum) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryGroup";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryGroup(start, end, (List<QueryControl>)(deepCopyList(controls, start, end)), 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)), (List<QGKey>)(deepCopyList(keys, start, end)), (List<QGAccum>)(deepCopyList(accums, start, end)));

return copy;
}

public boolean explike(){return true;}
}