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
public  class Spawn extends Cmd implements ComponentInfo, ProcMember {
 public  Id name; 
public final List<ProcMember> bits;
public Spawn(Token start, Token end, Id name, List<ProcMember> bits){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.bits = bits;
for(Object o : bits){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Spawn)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Spawn)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Spawn)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Spawn)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Spawn)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(ComponentInfoVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Spawn)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ComponentInfoWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Spawn)this, arg);
}
public String details(){
return "Spawn("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "bits=" + "["+Syntax.sepDetails(this.bits, ",")+"]"
    +")";
}
public static String detstr(
Object name, Object bits
) {
return "Spawn(" + 
"name=" + name+" "+"bits=" + bits
+ ")";}
public String toString() {return  "spawn" 
      + (name == null ? "" : " " + name + "")
      + "{" + sepStmt(bits, " ") + "} " ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; ; break;
                     case 1: ret = this.bits.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              if (name != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : bits
              for(int i = 0; i < this.bits.size(); i++) {                 ptrs.add(new SynPtr(this, 1, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.bits.set(index, (ProcMember) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Spawn";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Spawn(start, end, 
(name == null ? null : (Id)name.internalDeepCopy(start,end)), (List<ProcMember>)(deepCopyList(bits, start, end)));

return copy;
}


    public List<ProcMember> bits() {return bits;}

    public List<LocalMember> localMembers = new ArrayList<LocalMember>();
    public List<HighLevelCommunication> highLevelCommunications 
      = new ArrayList<HighLevelCommunication>(); 
    public ProcBody body = null;
    public ProcInit init = null;
    public List<FunDecl> funs = new ArrayList<FunDecl>();
    
    public List<LocalMember> localMembers() { return localMembers; }
    public List<HighLevelCommunication> highLevelCommunications() {
      return highLevelCommunications;
      }
    public ProcBody body() {return body;}
    public ProcInit init() {return init;}
    public List<FunDecl> funs() {return funs; }
    public Id name() {return name;}
    public Set<Seal> freeVarSeals = new HashSet<Seal>();
    public Set<Seal> freeVarSeals() {return freeVarSeals;}

    private Id serveId = fisher.desugar.DesugarUtils.gensym(this.start, "serve");
    public Id serveId() {return serveId;}
    
  
}