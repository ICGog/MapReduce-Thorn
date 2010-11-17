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
public  class FunBody extends Syntax implements Puretic {
public final List<MonoBody> funbodies;
 public  boolean isMarkedPure; 
public FunBody(Token start, Token end, List<MonoBody> funbodies, boolean isMarkedPure){
    super(start, end);
    this.funbodies = funbodies;
for(Object o : funbodies){children.add((Syntax)o);}
    this.isMarkedPure = isMarkedPure;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((FunBody)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((FunBody)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(PureticVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((FunBody)this, arg);
}
public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((FunBody)this, arg);
}
public String details(){
return "FunBody("
    + "funbodies=" + "["+Syntax.sepDetails(this.funbodies, ",")+"]"
    + " "
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    +")";
}
public static String detstr(
Object funbodies, Object isMarkedPure
) {
return "FunBody(" + 
"funbodies=" + funbodies+" "+"isMarkedPure=" + isMarkedPure
+ ")";}
public String toString() {return  sep(funbodies, " | ") ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.funbodies.get(index); ; break;
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
            
              // 0 : funbodies
              for(int i = 0; i < this.funbodies.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              // 1 : isMarkedPure
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.funbodies.set(index, (MonoBody) newval); break;
		 case 1: this.isMarkedPure = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "FunBody";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new FunBody(start, end, (List<MonoBody>)(deepCopyList(funbodies, start, end)), isMarkedPure);

return copy;
}

        public boolean isMarkedPure() {return this.isMarkedPure;}
     

     public Id id() {
       return funbodies.get(0).id;
     }
     public String funBodyForExp() {
       return sepExp(funbodies, " | ");
     }
     public Id idOfName(){return this.id();}
     

     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     
}