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
public  class QualName extends Syntax implements ProcMember {
public final List<Id> ids;
public QualName(Token start, Token end, List<Id> ids){
    super(start, end);
    this.ids = ids;
for(Object o : ids){children.add((Syntax)o);}
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((QualName)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((QualName)this, arg);
}







public String details(){
return "QualName("
    + "ids=" + "["+Syntax.sepDetails(this.ids, ",")+"]"
    +")";
}
public static String detstr(
Object ids
) {
return "QualName(" + 
"ids=" + ids
+ ")";}
public String toString() {return  sep(ids, ".") ;}

 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.ids.get(index); ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : ids
              for(int i = 0; i < this.ids.size(); i++) {                 ptrs.add(new SynPtr(this, 0, i));}
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.ids.set(index, (Id) newval); break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "QualName";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new QualName(start, end, (List<Id>)(deepCopyList(ids, start, end)));

return copy;
}


     public QualName butlast() {
        List<Id> nids = new ArrayList(ids.size()-1);
        if (ids.size() < 2) {
          throw new RuntimeException("Can't take butlast of empty or singleton QualName.");
        }
        Id last = null;
        for(int i = 0; i < ids.size()-1; i++) {
          last = ids.get(i);
          nids.add(last);
          }
        return new QualName(this.start, last.end, nids);        
     }
     

     public Id last() { return ids.get(ids.size()-1);}
     

     public boolean equals(Object o) {
       if(o instanceof QualName) {
         QualName qo = (QualName) o;
         return (this.ids.equals(qo.ids));
       }
       else return false;
     }
     public int hashCode(){
       int i = 0;
       for(Id id : this.ids) i += id.hashCode();
       return i;
     }
     
}