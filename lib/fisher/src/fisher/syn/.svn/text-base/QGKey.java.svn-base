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
public  class QGKey extends Syntax implements ProcMember {
 public  Id id; 
 public  Cmd init; 
public QGKey(Token start, Token end, Id id, Cmd init){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
    this.init = init;
    if(init != null) children.add(init);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QGKey)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QGKey)this, arg);
}







public String details(){
return "QGKey("
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "init=" + Syntax.detailsome(this.init)
    +")";
}
public static String detstr(
Object id, Object init
) {
return "QGKey(" + 
"id=" + id+" "+"init=" + init
+ ")";}
public String toString() {return  id + "=" + init ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.init; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (init) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : init
              if (init != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;
		 case 1: this.init = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QGKey";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QGKey(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), 
(init == null ? null : (Cmd)init.internalDeepCopy(start,end)));

return copy;
}


}