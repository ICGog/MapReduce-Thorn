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
public  class While extends LabellableLoop implements ProcMember {
 public  Cmd test; 
 public  Cmd body; 
 public  boolean reallyUntil; 
 public  boolean reallyDo; 
public While(Token start, Token end, Id label, Cmd test, Cmd body, boolean reallyUntil, boolean reallyDo){
    super(start, end, label);
    this.test = test;
    if(test != null) children.add(test);
    this.body = body;
    if(body != null) children.add(body);
    this.reallyUntil = reallyUntil;

    this.reallyDo = reallyDo;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((While)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((While)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((While)this, arg);
}






public String details(){
return "While("
    + "label=" + Syntax.detailsome(this.label)
    + " "
    + "test=" + Syntax.detailsome(this.test)
    + " "
    + "body=" + Syntax.detailsome(this.body)
    + " "
    + "un=" + Syntax.detailsome(this.reallyUntil)
    + " "
    + "do=" + Syntax.detailsome(this.reallyDo)
    +")";
}
public static String detstr(
Object label, Object test, Object body, Object reallyUntil, Object reallyDo
) {
return "While(" + 
"label=" + label+" "+"test=" + test+" "+"body=" + body+" "+"un=" + reallyUntil+" "+"do=" + reallyDo
+ ")";}
public String toString() {return this.mystr();}




 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.label; ; break;
                     case 1: ret = this.test; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (test) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.reallyUntil; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (reallyUntil) was null", this, field, index);}                
            ; break;
                     case 4: ret = this.reallyDo; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (reallyDo) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : label
              if (label != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : test
              if (test != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : body
              if (body != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : reallyUntil
              // 4 : reallyDo
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.label = (Id) newval; break;
		 case 1: this.test = (Cmd) newval; break;
		 case 2: this.body = (Cmd) newval; break;
		 case 3: this.reallyUntil = (Boolean) newval; break;
		 case 4: this.reallyDo = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "While";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new While(start, end, 
(label == null ? null : (Id)label.internalDeepCopy(start,end)), 
(test == null ? null : (Cmd)test.internalDeepCopy(start,end)), 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)), reallyUntil, reallyDo);

return copy;
}


       private String mystr() {
         String lbl = (label == null ? "" : label + ": ");
         String ctl = (reallyUntil ? "until(" : "while(") + test + ")";
         if (reallyDo) 
            return lbl +  "do " + body + " " + ctl;
         else
            return lbl + ctl + body;
       }
       
    
}