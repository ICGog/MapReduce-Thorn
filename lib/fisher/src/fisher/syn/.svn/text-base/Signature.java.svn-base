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
public  class Signature extends LabellableLoop implements ProcMember {
 public  int signature; 
 public  Cmd body; 
public Signature(Token start, Token end, Id label, int signature, Cmd body){
    super(start, end, label);
    this.signature = signature;

    this.body = body;
    if(body != null) children.add(body);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Signature)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Signature)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Signature)this, arg);
}






public String details(){
return "Signature("
    + "label=" + Syntax.detailsome(this.label)
    + " "
    + "signature=" + Syntax.detailsome(this.signature)
    + " "
    + "body=" + Syntax.detailsome(this.body)
    +")";
}
public static String detstr(
Object label, Object signature, Object body
) {
return "Signature(" + 
"label=" + label+" "+"signature=" + signature+" "+"body=" + body
+ ")";}
public String toString() {return 
     (label == null ? "" : label + ":")
   + "signature(" + signature + ")" + body
   ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.label; ; break;
                     case 1: ret = this.signature; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (signature) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : label
              if (label != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : signature
              // 2 : body
              if (body != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.label = (Id) newval; break;
		 case 1: this.signature = (Integer) newval; break;
		 case 2: this.body = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Signature";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Signature(start, end, 
(label == null ? null : (Id)label.internalDeepCopy(start,end)), signature, 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)));

return copy;
}


}