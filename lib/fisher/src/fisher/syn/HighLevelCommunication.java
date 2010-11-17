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
public abstract class HighLevelCommunication extends Syntax implements ProcMember {
 public  Id name; 
 public  FunBody funbody; 
public HighLevelCommunication(Token start, Token end, Id name, FunBody funbody){
    super(start, end);
    this.name = name;
    if(name != null) children.add(name);
    this.funbody = funbody;
    if(funbody != null) children.add(funbody);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((HighLevelCommunication)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((HighLevelCommunication)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ProcMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((HighLevelCommunication)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ProcMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((HighLevelCommunication)this, arg);
}
public String details(){
return "HighLevelCommunication("
    + "name=" + Syntax.detailsome(this.name)
    + " "
    + "funbody=" + Syntax.detailsome(this.funbody)
    +")";
}
public static String detstr(
Object name, Object funbody
) {
return "HighLevelCommunication(" + 
"name=" + name+" "+"funbody=" + funbody
+ ")";}
public String toString() {return this.details();}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.name; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (name) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.funbody; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (funbody) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : name
              if (name != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : funbody
              if (funbody != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.name = (Id) newval; break;
		 case 1: this.funbody = (FunBody) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "HighLevelCommunication";}
        



     public Id gennedFunName;
   
}