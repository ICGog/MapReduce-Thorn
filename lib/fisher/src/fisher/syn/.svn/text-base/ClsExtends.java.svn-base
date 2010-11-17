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
public  class ClsExtends extends Syntax implements ProcMember {
 public  QualName superName; 
public final List<Cmd> args;
public ClsExtends(Token start, Token end, QualName superName, List<Cmd> args){
    super(start, end);
    this.superName = superName;
    if(superName != null) children.add(superName);
    this.args = args;
for(Object o : args){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ClsExtends)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ClsExtends)this, arg);
}







public String details(){
return "ClsExtends("
    + "superName=" + Syntax.detailsome(this.superName)
    + " "
    + "args=" + "["+Syntax.sepDetails(this.args, ",")+"]"
    +")";
}
public static String detstr(
Object superName, Object args
) {
return "ClsExtends(" + 
"superName=" + superName+" "+"args=" + args
+ ")";}
public String toString() {return  superName + "(" + sep(args, ",") + ")";}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.superName; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (superName) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.args.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : superName
              if (superName != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : args
              for(int i = 0; i < this.args.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.superName = (QualName) newval; break;
		 case 1: this.args.set(index, (Cmd) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ClsExtends";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ClsExtends(start, end, 
(superName == null ? null : (QualName)superName.internalDeepCopy(start,end)), (List<Cmd>)(deepCopyList(args, start, end)));

return copy;
}


     public Seal superclassSeal;
   
}