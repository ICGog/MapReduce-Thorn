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
public  class For extends LabellableLoop implements ProcMember {
 public  Pat pat; 
 public  Cmd list; 
 public  Cmd body; 
 public  boolean inquisitive; 
public For(Token start, Token end, Id label, Pat pat, Cmd list, Cmd body, boolean inquisitive){
    super(start, end, label);
    this.pat = pat;
    if(pat != null) children.add(pat);
    this.list = list;
    if(list != null) children.add(list);
    this.body = body;
    if(body != null) children.add(body);
    this.inquisitive = inquisitive;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((For)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((For)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((For)this, arg);
}






public String details(){
return "For("
    + "label=" + Syntax.detailsome(this.label)
    + " "
    + "pat=" + Syntax.detailsome(this.pat)
    + " "
    + "list=" + Syntax.detailsome(this.list)
    + " "
    + "body=" + Syntax.detailsome(this.body)
    + " "
    + "inquisitive=" + Syntax.detailsome(this.inquisitive)
    +")";
}
public static String detstr(
Object label, Object pat, Object list, Object body, Object inquisitive
) {
return "For(" + 
"label=" + label+" "+"pat=" + pat+" "+"list=" + list+" "+"body=" + body+" "+"inquisitive=" + inquisitive
+ ")";}
public String toString() {return 
      (label == null ? "" : label + ": ") + 
      "for (" + pat + 
      (inquisitive ? " <~ " : " <- ")
      + list + ")" + stmt(body)
      ;}




 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.label; ; break;
                     case 1: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.list; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (list) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;
                     case 4: ret = this.inquisitive; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (inquisitive) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : label
              if (label != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : pat
              if (pat != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : list
              if (list != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : body
              if (body != null) ptrs.add(new SynPtr(this, 3, -1)); 
              // 4 : inquisitive
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.label = (Id) newval; break;
		 case 1: this.pat = (Pat) newval; break;
		 case 2: this.list = (Cmd) newval; break;
		 case 3: this.body = (Cmd) newval; break;
		 case 4: this.inquisitive = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "For";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new For(start, end, 
(label == null ? null : (Id)label.internalDeepCopy(start,end)), 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)), 
(list == null ? null : (Cmd)list.internalDeepCopy(start,end)), 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)), inquisitive);

return copy;
}


}