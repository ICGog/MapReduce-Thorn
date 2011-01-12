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
public  class AnonObj extends Cmd implements Classlike, Puretic {
public final List<ClsExtends> exts;
public final List<ObjectMember> members;
 public  boolean isMarkedPure; 
 public  Cmd actualCode; 
public AnonObj(Token start, Token end, List<ClsExtends> exts, List<ObjectMember> members, boolean isMarkedPure, Cmd actualCode){
    super(start, end);
    this.exts = exts;

    this.members = members;

    this.isMarkedPure = isMarkedPure;

    this.actualCode = actualCode;
    if(actualCode != null) children.add(actualCode);
}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonObj)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AnonObj)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(VisitCmd<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonObj)this, arg);
}






public <ARG, RET,  EXN extends Exception> RET accept(ClasslikeVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonObj)this, arg);
}
public <ARG,  EXN extends Exception> void accept(ClasslikeWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AnonObj)this, arg);
}
public <ARG, RET,  EXN extends Exception> RET accept(PureticVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((AnonObj)this, arg);
}
public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((AnonObj)this, arg);
}
public String details(){
return "AnonObj("
    + "exts=" + "["+Syntax.sepDetails(this.exts, ",")+"]"
    + " "
    + "members=" + "["+Syntax.sepDetails(this.members, ",")+"]"
    + " "
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    +")";
}
public static String detstr(
Object exts, Object members, Object isMarkedPure
) {
return "AnonObj(" + 
"exts=" + exts+" "+"members=" + members+" "+"isMarkedPure=" + isMarkedPure
+ ")";}
public String toString() {return 
      "object" + 
      (isMarkedPure ? ":pure " : "") +
      (exts.isEmpty() ? "" : "extends " + sep(exts, ","))
      + "{"
      + sepStmt(members, " ")
      + "} "
      ;}




 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.exts.get(index); ; break;
                     case 1: ret = this.members.get(index); ; break;
                     case 2: ret = this.isMarkedPure; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isMarkedPure) was null", this, field, index);}                
            ; break;
                     case 3: ret = this.actualCode; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (actualCode) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : exts
              // 1 : members
              // 2 : isMarkedPure
              // 3 : actualCode
              if (actualCode != null) ptrs.add(new SynPtr(this, 3, -1)); 
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.exts.set(index, (ClsExtends) newval); break;
		 case 1: this.members.set(index, (ObjectMember) newval); break;
		 case 2: this.isMarkedPure = (Boolean) newval; break;
		 case 3: this.actualCode = (Cmd) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "AnonObj";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new AnonObj(start, end, (List<ClsExtends>)(deepCopyList(exts, start, end)), (List<ObjectMember>)(deepCopyList(members, start, end)), isMarkedPure, 
(actualCode == null ? null : (Cmd)actualCode.internalDeepCopy(start,end)));

return copy;
}

        public boolean isMarkedPure() {return this.isMarkedPure;}
     
public boolean explike(){return true;}

     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     

     public ClassStatic classStatic;
     public SealForClass classSeal;
     public List<ClassFormal> params() {return Collections.EMPTY_LIST;}
     public boolean hasParams(){return false;}
     public Id name() {return null; }
     public List<ClassMember> members() {
        // OK, we're cheating on the type system here.
        return (List<ClassMember>) (List) this.members;
        }
     public void setClassStatic(ClassStatic cs) {
        this.classStatic = cs;
        }
     public List<ClsExtends> extendses() {return this.exts;}
     	
     public ClassStatic classStatic() {return classStatic;} 
     public SealForClass classSeal() {return classSeal;}
     public void setClassSeal(SealForClass classSeal) {
        this.classSeal = classSeal;
        }
     public Id idOfName(){return null;}
   
}