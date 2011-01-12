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
public  class QueryControlVar extends QueryControl implements ProcMember {
 public  Id var; 
 public  Cmd init; 
 public  Cmd next; 
 public  boolean doBeforeFirst; 
public QueryControlVar(Token start, Token end, Id var, Cmd init, Cmd next, boolean doBeforeFirst){
    super(start, end);
    this.var = var;
    if(var != null) children.add(var);
    this.init = init;
    if(init != null) children.add(init);
    this.next = next;
    if(next != null) children.add(next);
    this.doBeforeFirst = doBeforeFirst;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlVar)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryControlVar)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryControl<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlVar)this, arg);
}

public String details(){
return "QueryControlVar("
    + "var=" + Syntax.detailsome(this.var)
    + " "
    + "init=" + Syntax.detailsome(this.init)
    + " "
    + "next=" + Syntax.detailsome(this.next)
    + " "
    + "doBeforeFirst=" + Syntax.detailsome(this.doBeforeFirst)
    +")";
}
public static String detstr(
Object var, Object init, Object next, Object doBeforeFirst
) {
return "QueryControlVar(" + 
"var=" + var+" "+"init=" + init+" "+"next=" + next+" "+"doBeforeFirst=" + doBeforeFirst
+ ")";}
public String toString() {return 
     "var " + var + " := " + init 
     + (doBeforeFirst ? " %then0 " : " %then1 ")
     + next     
   ;}




 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.var; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (var) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.init; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (init) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.next; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (next) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.doBeforeFirst; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (doBeforeFirst) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : var
              if (var != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : init
              if (init != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : next
              if (next != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : doBeforeFirst
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.var = (Id) newval; break;
		 case 1: this.init = (Cmd) newval; break;
		 case 2: this.next = (Cmd) newval; break;
		 case 3: this.doBeforeFirst = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryControlVar";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryControlVar(start, end, 
(var == null ? null : (Id)var.internalDeepCopy(start,end)), 
(init == null ? null : (Cmd)init.internalDeepCopy(start,end)), 
(next == null ? null : (Cmd)next.internalDeepCopy(start,end)), doBeforeFirst);

return copy;
}


}