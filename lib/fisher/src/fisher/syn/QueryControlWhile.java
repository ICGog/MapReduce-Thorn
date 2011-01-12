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
public  class QueryControlWhile extends QueryControl implements ProcMember {
 public  Cmd test; 
 public  boolean isUntil; 
public QueryControlWhile(Token start, Token end, Cmd test, boolean isUntil){
    super(start, end);
    this.test = test;
    if(test != null) children.add(test);
    this.isUntil = isUntil;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlWhile)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryControlWhile)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryControl<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlWhile)this, arg);
}

public String details(){
return "QueryControlWhile("
    + "test=" + Syntax.detailsome(this.test)
    + " "
    + "isUntil=" + Syntax.detailsome(this.isUntil)
    +")";
}
public static String detstr(
Object test, Object isUntil
) {
return "QueryControlWhile(" + 
"test=" + test+" "+"isUntil=" + isUntil
+ ")";}
public String toString() {return  (isUntil ? "until " : "while ") + test ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.test; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (test) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.isUntil; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isUntil) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : test
              if (test != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : isUntil
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.test = (Cmd) newval; break;
		 case 1: this.isUntil = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryControlWhile";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryControlWhile(start, end, 
(test == null ? null : (Cmd)test.internalDeepCopy(start,end)), isUntil);

return copy;
}


}