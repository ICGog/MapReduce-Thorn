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
public  class PatListBitExp extends PatListBit implements ProcMember {
 public  Pat pat; 
public PatListBitExp(Token start, Token end, Pat pat){
    super(start, end);
    this.pat = pat;
    if(pat != null) children.add(pat);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatListBitExp)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatListBitExp)this, arg);
}







public String details(){
return "PatListBitExp("
    + "pat=" + Syntax.detailsome(this.pat)
    +")";
}
public static String detstr(
Object pat
) {
return "PatListBitExp(" + 
"pat=" + pat
+ ")";}
public String toString() {return  pat.toString() ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.pat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (pat) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : pat
              if (pat != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.pat = (Pat) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatListBitExp";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatListBitExp(start, end, 
(pat == null ? null : (Pat)pat.internalDeepCopy(start,end)));

return copy;
}


}