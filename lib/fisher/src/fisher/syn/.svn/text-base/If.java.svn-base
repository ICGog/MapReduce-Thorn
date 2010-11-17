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
public  class If extends Cmd implements ProcMember {
 public  Cmd test; 
 public  Cmd Then; 
 public  Cmd Else; 
 public  boolean reallyUnless; 
 public  boolean isExp; 
public If(Token start, Token end, Cmd test, Cmd Then, Cmd Else, boolean reallyUnless, boolean isExp){
    super(start, end);
    this.test = test;
    if(test != null) children.add(test);
    this.Then = Then;
    if(Then != null) children.add(Then);
    this.Else = Else;
    if(Else != null) children.add(Else);
    this.reallyUnless = reallyUnless;

    this.isExp = isExp;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((If)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((If)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((If)this, arg);
}






public String details(){
return "If("
    + "test=" + Syntax.detailsome(this.test)
    + " "
    + "Then=" + Syntax.detailsome(this.Then)
    + " "
    + "Else=" + Syntax.detailsome(this.Else)
    + " "
    + "un=" + Syntax.detailsome(this.reallyUnless)
    + " "
    + "isExp=" + Syntax.detailsome(this.isExp)
    +")";
}
public static String detstr(
Object test, Object Then, Object Else, Object reallyUnless, Object isExp
) {
return "If(" + 
"test=" + test+" "+"Then=" + Then+" "+"Else=" + Else+" "+"un=" + reallyUnless+" "+"isExp=" + isExp
+ ")";}
public String toString() {return 
     isExp ? (
       asExp(Then)
       + (reallyUnless ? "unless " : "if ") + test 
       + " else " 
       + asExp(Else)
      )
    : (
       (reallyUnless ? "unless (" : "if (") + test + ")" 
        + asStmt(Then) + ""
      + (Else == null ? "" : " else " + asStmt(Else) + "")
      )
    ;}





 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.test; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (test) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.Then; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (Then) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.Else; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (Else) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.reallyUnless; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (reallyUnless) was null", this, field, index);}                
            ; break;
                     case 4: ret = this.isExp; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isExp) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : test
              if (test != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : Then
              if (Then != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : Else
              if (Else != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : reallyUnless
              // 4 : isExp
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.test = (Cmd) newval; break;
		 case 1: this.Then = (Cmd) newval; break;
		 case 2: this.Else = (Cmd) newval; break;
		 case 3: this.reallyUnless = (Boolean) newval; break;
		 case 4: this.isExp = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "If";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new If(start, end, 
(test == null ? null : (Cmd)test.internalDeepCopy(start,end)), 
(Then == null ? null : (Cmd)Then.internalDeepCopy(start,end)), 
(Else == null ? null : (Cmd)Else.internalDeepCopy(start,end)), reallyUnless, isExp);

return copy;
}


}