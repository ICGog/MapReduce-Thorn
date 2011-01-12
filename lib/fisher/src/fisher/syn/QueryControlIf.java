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
public  class QueryControlIf extends QueryControl implements ProcMember {
 public  Cmd pred; 
public QueryControlIf(Token start, Token end, Cmd pred){
    super(start, end);
    this.pred = pred;
    if(pred != null) children.add(pred);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlIf)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryControlIf)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryControl<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlIf)this, arg);
}

public String details(){
return "QueryControlIf("
    + "pred=" + Syntax.detailsome(this.pred)
    +")";
}
public static String detstr(
Object pred
) {
return "QueryControlIf(" + 
"pred=" + pred
+ ")";}
public String toString() {return  "if " + pred;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.pred; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pred) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : pred
              if (pred != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.pred = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryControlIf";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryControlIf(start, end, 
(pred == null ? null : (Cmd)pred.internalDeepCopy(start,end)));

return copy;
}


}