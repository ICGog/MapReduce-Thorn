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
public  class StringBitText extends AbstractStringBit implements ProcMember {
 public  String value; 
public StringBitText(Token start, Token end, String value){
    super(start, end);
    this.value = value;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((StringBitText)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((StringBitText)this, arg);
}







public String details(){
return "StringBitText("
    + "value=" + Syntax.detailsome(this.value)
    +")";
}
public static String detstr(
Object value
) {
return "StringBitText(" + 
"value=" + value
+ ")";}
public String toString() {return "'"+value.toString()+"'";}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.value; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (value) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : value
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.value = (String) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "StringBitText";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new StringBitText(start, end, value);

return copy;
}


        public void formatted(QuoteStyle qs, StringBuffer sb) { qs.formatString(value, sb); }
     
}