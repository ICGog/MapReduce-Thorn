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
public  class ModuleFileAlias extends Syntax implements ModuleFileMember {
 public  Alias ali; 
public ModuleFileAlias(Token start, Token end, Alias ali){
    super(start, end);
    this.ali = ali;
    if(ali != null) children.add(ali);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileAlias)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileAlias)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(ModuleFileMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((ModuleFileAlias)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ModuleFileMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((ModuleFileAlias)this, arg);
}
public String details(){
return "ModuleFileAlias("
    + "ali=" + Syntax.detailsome(this.ali)
    +")";
}
public static String detstr(
Object ali
) {
return "ModuleFileAlias(" + 
"ali=" + ali
+ ")";}
public String toString() {return  ali.toString() ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.ali; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (ali) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : ali
              if (ali != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.ali = (Alias) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "ModuleFileAlias";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new ModuleFileAlias(start, end, 
(ali == null ? null : (Alias)ali.internalDeepCopy(start,end)));

return copy;
}


}