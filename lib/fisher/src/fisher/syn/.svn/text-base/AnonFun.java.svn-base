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
public  class AnonFun extends Cmd implements Puretic {
 public  FunBody fun; 
 public  boolean isMarkedPure; 
public AnonFun(Token start, Token end, FunBody fun, boolean isMarkedPure){
    super(start, end);
    this.fun = fun;
    if(fun != null) children.add(fun);
    this.isMarkedPure = isMarkedPure;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonFun)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AnonFun)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonFun)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(PureticVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonFun)this, arg);
}
public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AnonFun)this, arg);
}
public String details(){
return "AnonFun("
    + "fun=" + Syntax.detailsome(this.fun)
    + " "
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    +")";
}
public static String detstr(
Object fun, Object isMarkedPure
) {
return "AnonFun(" + 
"fun=" + fun+" "+"isMarkedPure=" + isMarkedPure
+ ")";}
public String toString() {return  "fn " + fun.funBodyForExp() ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.fun; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (fun) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.isMarkedPure; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isMarkedPure) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : fun
              if (fun != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : isMarkedPure
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.fun = (FunBody) newval; break;
		 case 1: this.isMarkedPure = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "AnonFun";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new AnonFun(start, end, 
(fun == null ? null : (FunBody)fun.internalDeepCopy(start,end)), isMarkedPure);

return copy;
}

        public boolean isMarkedPure() {return this.isMarkedPure;}
     
public boolean explike(){return true;}

     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     

     public Id idOfName() {return null;}
   
}