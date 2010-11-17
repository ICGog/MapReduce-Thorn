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
public  class QueryControlFor extends QueryControl implements ProcMember {
 public  Pat pat; 
 public  Cmd list; 
 public  boolean inquisitive; 
public QueryControlFor(Token start, Token end, Pat pat, Cmd list, boolean inquisitive){
    super(start, end);
    this.pat = pat;
    if(pat != null) children.add(pat);
    this.list = list;
    if(list != null) children.add(list);
    this.inquisitive = inquisitive;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlFor)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QueryControlFor)this, arg);
}





public <ARG, RET,  EXN extends Exception> RET accept(VisitQueryControl<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QueryControlFor)this, arg);
}

public String details(){
return "QueryControlFor("
    + "pat=" + Syntax.detailsome(this.pat)
    + " "
    + "list=" + Syntax.detailsome(this.list)
    + " "
    + "inquisitive=" + Syntax.detailsome(this.inquisitive)
    +")";
}
public static String detstr(
Object pat, Object list, Object inquisitive
) {
return "QueryControlFor(" + 
"pat=" + pat+" "+"list=" + list+" "+"inquisitive=" + inquisitive
+ ")";}
public String toString() {return  "for " + pat +
      (inquisitive ? " <~ " : " <- ")
      + list ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.list; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (list) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.inquisitive; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (inquisitive) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : pat
              if (pat != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : list
              if (list != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : inquisitive
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.pat = (Pat) newval; break;
		 case 1: this.list = (Cmd) newval; break;
		 case 2: this.inquisitive = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QueryControlFor";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QueryControlFor(start, end, 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)), 
(list == null ? null : (Cmd)list.internalDeepCopy(start,end)), inquisitive);

return copy;
}


}