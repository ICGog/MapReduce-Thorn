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
public abstract class Pat extends Syntax implements ProcMember {

public Pat(Token start, Token end){
    super(start, end);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Pat)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Pat)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Pat)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((Pat)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Pat)this, arg);
}


public String details(){
return "Pat("
    +")";
}
public static String detstr(

) {
return "Pat(" + 
""
+ ")";}
public String toString() {return this.details();}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Pat";}
        



}