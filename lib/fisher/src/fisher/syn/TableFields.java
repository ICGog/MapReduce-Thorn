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
public  class TableFields extends Syntax implements TableMember {
 public  ColAccess colAccess; 
 public  ColSpecial colSpecial; 
public final List<IdWithOptInit> idInits;
public TableFields(Token start, Token end, ColAccess colAccess, ColSpecial colSpecial, List<IdWithOptInit> idInits){
    super(start, end);
    this.colAccess = colAccess;

    this.colSpecial = colSpecial;

    this.idInits = idInits;
for(Object o : idInits){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TableFields)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((TableFields)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(TableMemberVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((TableFields)this, arg);
}
public <ARG,  EXN extends Exception> void accept(TableMemberWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((TableFields)this, arg);
}
public String details(){
return "TableFields("
    + "colAccess=" + Syntax.detailsome(this.colAccess)
    + " "
    + "colSpecial=" + Syntax.detailsome(this.colSpecial)
    + " "
    + "idInits=" + "["+Syntax.sepDetails(this.idInits, ",")+"]"
    +")";
}
public static String detstr(
Object colAccess, Object colSpecial, Object idInits
) {
return "TableFields(" + 
"colAccess=" + colAccess+" "+"colSpecial=" + colSpecial+" "+"idInits=" + idInits
+ ")";}
public String toString() {return 
      colSpecial.asPrefix() + colAccess + " " + sep(idInits, ", ") + ";"
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
                     case 2: ret = this.idInits.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : colAccess
              // 1 : colSpecial
              // 2 : idInits
              for(int i = 0; i < this.idInits.size(); i++) {                 ptrs.add(new SynPtr(this, 2, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.colAccess = (ColAccess) newval; break;
		 case 1: this.colSpecial = (ColSpecial) newval; break;
		 case 2: this.idInits.set(index, (IdWithOptInit) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "TableFields";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new TableFields(start, end, colAccess, colSpecial, (List<IdWithOptInit>)(deepCopyList(idInits, start, end)));

return copy;
}


}