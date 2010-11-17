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
public  class MatchExp extends Cmd implements ProcMember {
 public  Cmd subject; 
 public  Pat pat; 
public MatchExp(Token start, Token end, Cmd subject, Pat pat){
    super(start, end);
    this.subject = subject;
    if(subject != null) children.add(subject);
    this.pat = pat;
    if(pat != null) children.add(pat);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MatchExp)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MatchExp)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MatchExp)this, arg);
}






public String details(){
return "MatchExp("
    + "subject=" + Syntax.detailsome(this.subject)
    + " "
    + "pat=" + Syntax.detailsome(this.pat)
    +")";
}
public static String detstr(
Object subject, Object pat
) {
return "MatchExp(" + 
"subject=" + subject+" "+"pat=" + pat
+ ")";}
public String toString() {return  subject + " ~ " + pat ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.subject; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (subject) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : subject
              if (subject != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : pat
              if (pat != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.subject = (Cmd) newval; break;
		 case 1: this.pat = (Pat) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "MatchExp";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new MatchExp(start, end, 
(subject == null ? null : (Cmd)subject.internalDeepCopy(start,end)), 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)));

return copy;
}


}