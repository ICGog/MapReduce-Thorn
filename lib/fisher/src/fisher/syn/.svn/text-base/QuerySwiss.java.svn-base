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
public  class QuerySwiss extends QueryAbstract implements ProcMember {
 public  Cmd one; 
 public  Cmd more; 
 public  Cmd none; 
public QuerySwiss(Token start, Token end, List<QueryControl> controls, Cmd actualCode, Cmd one, Cmd more, Cmd none){
    super(start, end, controls, actualCode);
    this.one = one;

    this.more = more;

    this.none = none;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QuerySwiss)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QuerySwiss)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QuerySwiss)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryAbstract<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QuerySwiss)this, arg);
}
public String details(){
return "QuerySwiss("
    + "controls=" + "["+Syntax.sepDetails(this.controls, ",")+"]"
    + " "
    + "one=" + Syntax.detailsome(this.one)
    + " "
    + "more=" + Syntax.detailsome(this.more)
    + " "
    + "none=" + Syntax.detailsome(this.none)
    +")";
}
public static String detstr(
Object controls, Object one, Object more, Object none
) {
return "QuerySwiss(" + 
"controls=" + controls+" "+"one=" + one+" "+"more=" + more+" "+"none=" + none
+ ")";}
public String toString() {return 
     "%(" + one
     + (more == null ? "" : " %> " + more)
     + (none == null ? "" : " %< " + none)
     + super.toString()
     + ")"
   ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.controls.get(index); ; break;
                     case 1: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.one; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (one) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.more; ; break;
                     case 4: ret = this.none; ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : controls
              // 1 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : one
              // 3 : more
              // 4 : none
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.controls.set(index, (QueryControl) newval); break;
		 case 1: this.actualCode = (Cmd) newval; break;
		 case 2: this.one = (Cmd) newval; break;
		 case 3: this.more = (Cmd) newval; break;
		 case 4: this.none = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QuerySwiss";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QuerySwiss(start, end, (List<QueryControl>)(deepCopyList(controls, start, end)), 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)), 
(one == null ? null : (Cmd)one.internalDeepCopy(start,end)), 
(more == null ? null : (Cmd)more.internalDeepCopy(start,end)), 
(none == null ? null : (Cmd)none.internalDeepCopy(start,end)));

return copy;
}


}