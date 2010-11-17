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
public  class MethodCall extends Cmd implements ProcMember {
 public  Cmd target; 
 public  Id method; 
public final List<Cmd> args;
public MethodCall(Token start, Token end, Cmd target, Id method, List<Cmd> args){
    super(start, end);
    this.target = target;
    if(target != null) children.add(target);
    this.method = method;
    if(method != null) children.add(method);
    this.args = args;
for(Object o : args){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethodCall)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MethodCall)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MethodCall)this, arg);
}






public String details(){
return "MethodCall("
    + "target=" + Syntax.detailsome(this.target)
    + " "
    + "method=" + Syntax.detailsome(this.method)
    + " "
    + "args=" + "["+Syntax.sepDetails(this.args, ",")+"]"
    +")";
}
public static String detstr(
Object target, Object method, Object args
) {
return "MethodCall(" + 
"target=" + target+" "+"method=" + method+" "+"args=" + args
+ ")";}
public String toString() {return target + "." + method + "(" + sep(args, ", ") + ")" ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.target; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (target) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.method; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (method) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.args.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : target
              if (target != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : method
              if (method != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : args
              for(int i = 0; i < this.args.size(); i++) {                 ptrs.add(new SynPtr(this, 2, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.target = (Cmd) newval; break;
		 case 1: this.method = (Id) newval; break;
		 case 2: this.args.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "MethodCall";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new MethodCall(start, end, 
(target == null ? null : (Cmd)target.internalDeepCopy(start,end)), 
(method == null ? null : (Id)method.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(args, start, end)));

     ((MethodCall)copy).sig = this.sig;
   
return copy;
}

public boolean explike(){return true;}

     public MethodSig sig;
   
}