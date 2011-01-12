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
public  class QGAccum extends Syntax implements ProcMember {
 public  ColAccess colAccess; 
 public  ColSpecial colSpecial; 
 public  Id id; 
 public  Cmd first; 
 public  Cmd then; 
 public  Cmd after; 
public QGAccum(Token start, Token end, ColAccess colAccess, ColSpecial colSpecial, Id id, Cmd first, Cmd then, Cmd after){
    super(start, end);
    this.colAccess = colAccess;

    this.colSpecial = colSpecial;

    this.id = id;
    if(id != null) children.add(id);
    this.first = first;
    if(first != null) children.add(first);
    this.then = then;
    if(then != null) children.add(then);
    this.after = after;
    if(after != null) children.add(after);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QGAccum)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QGAccum)this, arg);
}







public String details(){
return "QGAccum("
    + "colAccess=" + Syntax.detailsome(this.colAccess)
    + " "
    + "colSpecial=" + Syntax.detailsome(this.colSpecial)
    + " "
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "first=" + Syntax.detailsome(this.first)
    + " "
    + "then=" + Syntax.detailsome(this.then)
    + " "
    + "after=" + Syntax.detailsome(this.after)
    +")";
}
public static String detstr(
Object colAccess, Object colSpecial, Object id, Object first, Object then, Object after
) {
return "QGAccum(" + 
"colAccess=" + colAccess+" "+"colSpecial=" + colSpecial+" "+"id=" + id+" "+"first=" + first+" "+"then=" + then+" "+"after=" + after
+ ")";}
public String toString() {return 
    colSpecial.asPrefix() + colAccess + " " 
    + id + " = " 
    + "%first " + first
    + " %then " + then
    + (after == null ? "" : " %after " + after )
    + ";"
  ;}






 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.colAccess; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (colAccess) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.colSpecial; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (colSpecial) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.id; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (id) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.first; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (first) was null", this, field, index);}                
            ; break;
                     case 4: ret = this.then; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (then) was null", this, field, index);}                
            ; break;
                     case 5: ret = this.after; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (after) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : colAccess
              // 1 : colSpecial
              // 2 : id
              if (id != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : first
              if (first != null) ptrs.add(new SynPtr(this, 3, -1)); 
              // 4 : then
              if (then != null) ptrs.add(new SynPtr(this, 4, -1)); 
              // 5 : after
              if (after != null) ptrs.add(new SynPtr(this, 5, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.colAccess = (ColAccess) newval; break;
		 case 1: this.colSpecial = (ColSpecial) newval; break;
		 case 2: this.id = (Id) newval; break;
		 case 3: this.first = (Cmd) newval; break;
		 case 4: this.then = (Cmd) newval; break;
		 case 5: this.after = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QGAccum";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QGAccum(start, end, colAccess, colSpecial, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), 
(first == null ? null : (Cmd)first.internalDeepCopy(start,end)), 
(then == null ? null : (Cmd)then.internalDeepCopy(start,end)), 
(after == null ? null : (Cmd)after.internalDeepCopy(start,end)));

return copy;
}


}