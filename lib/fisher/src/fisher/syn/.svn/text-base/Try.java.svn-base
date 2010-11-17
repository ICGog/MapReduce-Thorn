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
public  class Try extends Cmd implements ProcMember {
 public  Cmd body; 
public final List<Case> cases;
 public  Cmd fin; 
public Try(Token start, Token end, Cmd body, List<Case> cases, Cmd fin){
    super(start, end);
    this.body = body;
    if(body != null) children.add(body);
    this.cases = cases;
for(Object o : cases){children.add((Syntax)o);}
    this.fin = fin;
    if(fin != null) children.add(fin);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Try)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Try)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Try)this, arg);
}






public String details(){
return "Try("
    + "body=" + Syntax.detailsome(this.body)
    + " "
    + "cases=" + "["+Syntax.sepDetails(this.cases, ",")+"]"
    + " "
    + "fin=" + Syntax.detailsome(this.fin)
    +")";
}
public static String detstr(
Object body, Object cases, Object fin
) {
return "Try(" + 
"body=" + body+" "+"cases=" + cases+" "+"fin=" + fin
+ ")";}
public String toString() {return  "try " + body + 
              (cases.isEmpty() ? "" : " catch {" + sep(cases, " | ") + "} ")
              + (fin == null ? "" : " finally " + fin)
              ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.cases.get(index); ; break;
                     case 2: ret = this.fin; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (fin) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : body
              if (body != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : cases
              for(int i = 0; i < this.cases.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : fin
              if (fin != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.body = (Cmd) newval; break;
		 case 1: this.cases.set(index, (Case) newval); break;
		 case 2: this.fin = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Try";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Try(start, end, 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)), (List<Case>)(deepCopyList(cases, start, end)), 
(fin == null ? null : (Cmd)fin.internalDeepCopy(start,end)));

return copy;
}


}