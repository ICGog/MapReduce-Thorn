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
public  class SuperCtorCall extends SuperThingie implements ProcMember {

public SuperCtorCall(Token start, Token end, QualName cls, List<Cmd> args){
    super(start, end, cls, args);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SuperCtorCall)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((SuperCtorCall)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SuperCtorCall)this, arg);
}






public String details(){
return "SuperCtorCall("
    + "cls=" + Syntax.detailsome(this.cls)
    + " "
    + "args=" + "["+Syntax.sepDetails(this.args, ",")+"]"
    +")";
}
public static String detstr(
Object cls, Object args
) {
return "SuperCtorCall(" + 
"cls=" + cls+" "+"args=" + args
+ ")";}
public String toString() {return  
     (cls == null ? "new" : "new@" + cls)
     + "(" + sep(args, ", ") + ")"
   ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.cls; ; break;
                     case 1: ret = this.args.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : cls
              if (cls != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : args
              for(int i = 0; i < this.args.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.cls = (QualName) newval; break;
		 case 1: this.args.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "SuperCtorCall";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new SuperCtorCall(start, end, 
(cls == null ? null : (QualName)cls.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(args, start, end)));

return copy;
}


     public boolean isCallToADifferentCtorForThis() { 
        return cls == null;
     }
     public ClassStatic classStaticForCurrentClass = null; // only used if isCallToADifferentCtorForThis
   
}