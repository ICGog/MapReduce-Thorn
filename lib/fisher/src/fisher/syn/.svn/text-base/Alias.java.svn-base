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
public  class Alias extends Cmd implements ProcMember {
 public  QualName newname; 
 public  QualName oldname; 
public Alias(Token start, Token end, QualName newname, QualName oldname){
    super(start, end);
    this.newname = newname;
    if(newname != null) children.add(newname);
    this.oldname = oldname;
    if(oldname != null) children.add(oldname);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Alias)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((Alias)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((Alias)this, arg);
}






public String details(){
return "Alias("
    + "newname=" + Syntax.detailsome(this.newname)
    + " "
    + "oldname=" + Syntax.detailsome(this.oldname)
    +")";
}
public static String detstr(
Object newname, Object oldname
) {
return "Alias(" + 
"newname=" + newname+" "+"oldname=" + oldname
+ ")";}
public String toString() {return 
      "alias " + newname + " = " + oldname + ";"
      ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.newname; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (newname) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.oldname; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (oldname) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : newname
              if (newname != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : oldname
              if (oldname != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.newname = (QualName) newval; break;
		 case 1: this.oldname = (QualName) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "Alias";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new Alias(start, end, 
(newname == null ? null : (QualName)newname.internalDeepCopy(start,end)), 
(oldname == null ? null : (QualName)oldname.internalDeepCopy(start,end)));

return copy;
}


}