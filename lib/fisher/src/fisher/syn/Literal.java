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
public  class Literal extends Cmd implements ProcMember {
 public  Object value; 
public Literal(Token start, Token end, Object value){
    super(start, end);
    this.value = value;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Literal)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Literal)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Literal)this, arg);
}






public String details() {return "(" + ( value == null ? "<null>" : value.toString() ) + ")";}
public static String detstr(
Object value
) {
return "Literal(" + 
"value=" + value
+ ")";}
public String toString() {return 
     value == null ? "null" :
     value instanceof String ? quote(value.toString()) :  value.toString()
   ;}

 
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
        
		 case 0: this.value = (Object) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Literal";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Literal(start, end, value);

return copy;
}

public boolean explike(){return true;}

      public Thing thing;
      public static String quote(String s) {
         if (s.contains("\n")) return " + s + ";
         if (! s.contains("\"")) return "\"" + s + "\"";
         if (! s.contains("'")) return "'" + s +"'";
         return "\"" + s.replaceAll("\"", "\\\"") + "\"";
      }
   
}