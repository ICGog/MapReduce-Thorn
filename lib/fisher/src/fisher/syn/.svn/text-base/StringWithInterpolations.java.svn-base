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
public  class StringWithInterpolations extends Cmd implements ProcMember {
public final List<AbstractStringBit> bits;
 public  QuoteStyle quoteStyle; 
public StringWithInterpolations(Token start, Token end, List<AbstractStringBit> bits, QuoteStyle quoteStyle){
    super(start, end);
    this.bits = bits;
for(Object o : bits){children.add((Syntax)o);}
    this.quoteStyle = quoteStyle;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((StringWithInterpolations)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((StringWithInterpolations)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((StringWithInterpolations)this, arg);
}






public String details(){
return "StringWithInterpolations("
    + "bits=" + "["+Syntax.sepDetails(this.bits, ",")+"]"
    + " "
    + "quoteStyle=" + Syntax.detailsome(this.quoteStyle)
    +")";
}
public static String detstr(
Object bits, Object quoteStyle
) {
return "StringWithInterpolations(" + 
"bits=" + bits+" "+"quoteStyle=" + quoteStyle
+ ")";}
public String toString() {return quoteStyle.format(bits);}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.bits.get(index); ; break;
                     case 1: ret = this.quoteStyle; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (quoteStyle) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : bits
              for(int i = 0; i < this.bits.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              // 1 : quoteStyle
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.bits.set(index, (AbstractStringBit) newval); break;
		 case 1: this.quoteStyle = (QuoteStyle) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "StringWithInterpolations";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new StringWithInterpolations(start, end, (List<AbstractStringBit>)(deepCopyList(bits, start, end)), quoteStyle);

return copy;
}


}