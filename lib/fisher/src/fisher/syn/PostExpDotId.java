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
public  class PostExpDotId extends PostExp implements ProcMember {
 public  Id id; 
public PostExpDotId(Token start, Token end, Id id){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PostExpDotId)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PostExpDotId)this, arg);
}







public String details(){
return "PostExpDotId("
    + "id=" + Syntax.detailsome(this.id)
    +")";
}
public static String detstr(
Object id
) {
return "PostExpDotId(" + 
"id=" + id
+ ")";}
public String toString() {return  "." + id ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PostExpDotId";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PostExpDotId(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)));

return copy;
}


}