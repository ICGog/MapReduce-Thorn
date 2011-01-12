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
public  class Case extends Syntax implements ProcMember {
 public  Pat pat; 
 public  Pat from; 
 public  Pat envelope; 
 public  boolean hasPrio; 
 public  int prio; 
 public  boolean checked; 
 public  Cmd body; 
public Case(Token start, Token end, Pat pat, Pat from, Pat envelope, boolean hasPrio, int prio, boolean checked, Cmd body){
    super(start, end);
    this.pat = pat;
    if(pat != null) children.add(pat);
    this.from = from;
    if(from != null) children.add(from);
    this.envelope = envelope;
    if(envelope != null) children.add(envelope);
    this.hasPrio = hasPrio;

    this.prio = prio;

    this.checked = checked;

    this.body = body;
    if(body != null) children.add(body);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Case)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Case)this, arg);
}







public String details(){
return "Case("
    + "pat=" + Syntax.detailsome(this.pat)
    + " "
    + "from=" + Syntax.detailsome(this.from)
    + " "
    + "envelope=" + Syntax.detailsome(this.envelope)
    + " "
    + "hasPrio=" + Syntax.detailsome(this.hasPrio)
    + " "
    + "prio=" + Syntax.detailsome(this.prio)
    + " "
    + "checked=" + Syntax.detailsome(this.checked)
    + " "
    + "body=" + Syntax.detailsome(this.body)
    +")";
}
public static String detstr(
Object pat, Object from, Object envelope, Object hasPrio, Object prio, Object checked, Object body
) {
return "Case(" + 
"pat=" + pat+" "+"from=" + from+" "+"envelope=" + envelope+" "+"hasPrio=" + hasPrio+" "+"prio=" + prio+" "+"checked=" + checked+" "+"body=" + body
+ ")";}
public String toString() {return 
      pat 
      + (from == null ? "" : " from " + from )
      + (envelope == null ? "" : " envelope " + envelope )
      + (!hasPrio ? "" : " prio " + prio)
      + (checked ? " checked " : "")
      + " = " + asStmt(body)
   ;}







 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.from; ; break;
                     case 2: ret = this.envelope; ; break;
                     case 3: ret = this.hasPrio; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (hasPrio) was null", this, field, index);}                
            ; break;
                     case 4: ret = this.prio; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (prio) was null", this, field, index);}                
            ; break;
                     case 5: ret = this.checked; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (checked) was null", this, field, index);}                
            ; break;
                     case 6: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : pat
              if (pat != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : from
              if (from != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : envelope
              if (envelope != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : hasPrio
              // 4 : prio
              // 5 : checked
              // 6 : body
              if (body != null) ptrs.add(new SynPtr(this, 6, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.pat = (Pat) newval; break;
		 case 1: this.from = (Pat) newval; break;
		 case 2: this.envelope = (Pat) newval; break;
		 case 3: this.hasPrio = (Boolean) newval; break;
		 case 4: this.prio = (Integer) newval; break;
		 case 5: this.checked = (Boolean) newval; break;
		 case 6: this.body = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Case";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Case(start, end, 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)), 
(from == null ? null : (Pat)from.internalDeepCopy(start,end)), 
(envelope == null ? null : (Pat)envelope.internalDeepCopy(start,end)), hasPrio, prio, checked, 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)));

return copy;
}


}