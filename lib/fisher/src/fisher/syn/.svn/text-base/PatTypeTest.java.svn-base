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
public  class PatTypeTest extends Pat implements ProcMember {
 public  Pat subpat; 
 public  QualName shouldBe; 
public PatTypeTest(Token start, Token end, Pat subpat, QualName shouldBe){
    super(start, end);
    this.subpat = subpat;
    if(subpat != null) children.add(subpat);
    this.shouldBe = shouldBe;
    if(shouldBe != null) children.add(shouldBe);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatTypeTest)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatTypeTest)this, arg);
}

public <ARG, RET,  EXN extends Exception> RET accept(VisitPat<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((PatTypeTest)this, arg);
}
public <ARG1, ARG2, RET,  EXN extends Exception> RET accept(VisitPat2<ARG1, ARG2,RET,  EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {
   return vis.visit((PatTypeTest)this, arg1, arg2);
}

public <ARG,  EXN extends Exception> void accept(WalkPat<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((PatTypeTest)this, arg);
}


public String details(){
return "PatTypeTest("
    + "subpat=" + Syntax.detailsome(this.subpat)
    + " "
    + "shouldBe=" + Syntax.detailsome(this.shouldBe)
    +")";
}
public static String detstr(
Object subpat, Object shouldBe
) {
return "PatTypeTest(" + 
"subpat=" + subpat+" "+"shouldBe=" + shouldBe
+ ")";}
public String toString() {return  subpat + ":" + shouldBe ;}


 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.subpat; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (subpat) was null", this, field, index);}                
            ; break;
                     case 1: ret = this.shouldBe; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (shouldBe) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : subpat
              if (subpat != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : shouldBe
              if (shouldBe != null) ptrs.add(new SynPtr(this, 1, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.subpat = (Pat) newval; break;
		 case 1: this.shouldBe = (QualName) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "PatTypeTest";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new PatTypeTest(start, end, 
(subpat == null ? null : (Pat)subpat.internalDeepCopy(start,end)), 
(shouldBe == null ? null : (QualName)shouldBe.internalDeepCopy(start,end)));

return copy;
}


}