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
public  class ServeBlock extends Syntax implements ProcMember {
 public  String name; 
public final List<Id> formals;
 public  Cmd cmd; 
public ServeBlock(Token start, Token end, String name, List<Id> formals, Cmd cmd){
    super(start, end);
    this.name = name;

    this.formals = formals;
for(Object o : formals){children.add((Syntax)o);}
    this.cmd = cmd;
    if(cmd != null) children.add(cmd);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ServeBlock)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ServeBlock)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ServeBlock)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ServeBlock)this, arg);
}
public String details(){
return "ServeBlock("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "formals=" + "["+Syntax.sepDetails(this.formals, ",")+"]"
    + " "
    + "cmd=" + Syntax.detailsome(this.cmd)
    +")";
}
public static String detstr(
Object name, Object formals, Object cmd
) {
return "ServeBlock(" + 
"name=" + name+" "+"formals=" + formals+" "+"cmd=" + cmd
+ ")";}
public String toString() {return 
     name 
     + (formals.size() == 0 ? "" : "(" + sep(formals, ", ") + ")")
     + cmd        
   ;}



 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.formals.get(index); ; break;
                     case 2: ret = this.cmd; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (cmd) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              // 1 : formals
              for(int i = 0; i < this.formals.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              // 2 : cmd
              if (cmd != null) ptrs.add(new SynPtr(this, 2, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (String) newval; break;
		 case 1: this.formals.set(index, (Id) newval); break;
		 case 2: this.cmd = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ServeBlock";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ServeBlock(start, end, name, (List<Id>)(deepCopyList(formals, start, end)), 
(cmd == null ? null : (Cmd)cmd.internalDeepCopy(start,end)));

return copy;
}


}