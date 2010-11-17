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
public  class SuperCall extends SuperThingie implements ProcMember {
 public  Id method; 
public SuperCall(Token start, Token end, QualName cls, List<Cmd> args, Id method){
    super(start, end, cls, args);
    this.method = method;
    if(method != null) children.add(method);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SuperCall)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((SuperCall)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((SuperCall)this, arg);
}






public String details(){
return "SuperCall("
    + "cls=" + Syntax.detailsome(this.cls)
    + " "
    + "args=" + "["+Syntax.sepDetails(this.args, ",")+"]"
    + " "
    + "method=" + Syntax.detailsome(this.method)
    +")";
}
public static String detstr(
Object cls, Object args, Object method
) {
return "SuperCall(" + 
"cls=" + cls+" "+"args=" + args+" "+"method=" + method
+ ")";}
public String toString() {return 
      cls == null ? "super." + method + "(" + sep(args, ", ") + ")"
       : "super@" + cls + "." + method + "(" + sep(args, ", ") + ")"
      ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.cls; ; break;
                     case 1: ret = this.args.get(index); ; break;
                     case 2: ret = this.method; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (method) was null", this, field, index);}                
            ; break;

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
              // 2 : method
              if (method != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.cls = (QualName) newval; break;
		 case 1: this.args.set(index, (Cmd) newval); break;
		 case 2: this.method = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "SuperCall";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new SuperCall(start, end, 
(cls == null ? null : (QualName)cls.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(args, start, end)), 
(method == null ? null : (Id)method.internalDeepCopy(start,end)));

     ((SuperCall)copy).sig = this.sig;
   
return copy;
}

public boolean explike(){return true;}

     public MethodSig sig;
   
}