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
public  class MonoBody extends Syntax implements Puretic {
 public  Id id; 
 public  Formals formals; 
 public  Pat from; 
 public  Pat envelope; 
 public  boolean hasPrio; 
 public  int prio; 
 public  Cmd body; 
 public  boolean checked; 
 public  boolean isMarkedPure; 
public MonoBody(Token start, Token end, Id id, Formals formals, Pat from, Pat envelope, boolean hasPrio, int prio, Cmd body, boolean checked, boolean isMarkedPure){
    super(start, end);
    this.id = id;
    if(id != null) children.add(id);
    this.formals = formals;
    if(formals != null) children.add(formals);
    this.from = from;
    if(from != null) children.add(from);
    this.envelope = envelope;
    if(envelope != null) children.add(envelope);
    this.hasPrio = hasPrio;

    this.prio = prio;

    this.body = body;
    if(body != null) children.add(body);
    this.checked = checked;

    this.isMarkedPure = isMarkedPure;

}
public <ARG, RET,  EXN extends Exception> RET accept(Visitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MonoBody)this, arg);
}
public <ARG,  EXN extends Exception> void accept(Walker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MonoBody)this, arg);
}







public <ARG, RET,  EXN extends Exception> RET accept(PureticVisitor<ARG,RET,  EXN> vis, ARG arg) throws EXN {
   return vis.visit((MonoBody)this, arg);
}
public <ARG,  EXN extends Exception> void accept(PureticWalker<ARG, EXN> vis, ARG arg) throws EXN {
    vis.visit((MonoBody)this, arg);
}
public String details(){
return "MonoBody("
    + "id=" + Syntax.detailsome(this.id)
    + " "
    + "formals=" + Syntax.detailsome(this.formals)
    + " "
    + "from=" + Syntax.detailsome(this.from)
    + " "
    + "envelope=" + Syntax.detailsome(this.envelope)
    + " "
    + "hasPrio=" + Syntax.detailsome(this.hasPrio)
    + " "
    + "prio=" + Syntax.detailsome(this.prio)
    + " "
    + "body=" + Syntax.detailsome(this.body)
    + " "
    + "checked=" + Syntax.detailsome(this.checked)
    + " "
    + "isMarkedPure=" + Syntax.detailsome(this.isMarkedPure)
    +")";
}
public static String detstr(
Object id, Object formals, Object from, Object envelope, Object hasPrio, Object prio, Object body, Object checked, Object isMarkedPure
) {
return "MonoBody(" + 
"id=" + id+" "+"formals=" + formals+" "+"from=" + from+" "+"envelope=" + envelope+" "+"hasPrio=" + hasPrio+" "+"prio=" + prio+" "+"body=" + body+" "+"checked=" + checked+" "+"isMarkedPure=" + isMarkedPure
+ ")";}
public String toString() {return 
     (id == null ? "" : id +
       (formals.toString().startsWith("(") ? "" : " ")
       )
     + formals  
     + (from == null ? "" : " from " + from)
     + (envelope == null ? "" : " envelope " + envelope)
     + (hasPrio ? " prio " + prio : "")
     + (checked ? " checked " : "")
     + " = " + stmt(body);}









 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          
                     case 0: ret = this.id; ; break;
                     case 1: ret = this.formals; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (formals) was null", this, field, index);}                
            ; break;
                     case 2: ret = this.from; ; break;
                     case 3: ret = this.envelope; ; break;
                     case 4: ret = this.hasPrio; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (hasPrio) was null", this, field, index);}                
            ; break;
                     case 5: ret = this.prio; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (prio) was null", this, field, index);}                
            ; break;
                     case 6: ret = this.body; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (body) was null", this, field, index);}                
            ; break;
                     case 7: ret = this.checked; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (checked) was null", this, field, index);}                
            ; break;
                     case 8: ret = this.isMarkedPure; 
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (isMarkedPure) was null", this, field, index);}                
            ; break;

           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        

            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            
              // 0 : id
              if (id != null) ptrs.add(new SynPtr(this, 0, -1)); 
              // 1 : formals
              if (formals != null) ptrs.add(new SynPtr(this, 1, -1)); 
              // 2 : from
              if (from != null) ptrs.add(new SynPtr(this, 2, -1)); 
              // 3 : envelope
              if (envelope != null) ptrs.add(new SynPtr(this, 3, -1)); 
              // 4 : hasPrio
              // 5 : prio
              // 6 : body
              if (body != null) ptrs.add(new SynPtr(this, 6, -1)); 
              // 7 : checked
              // 8 : isMarkedPure
              return ptrs;
              }

        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        
		 case 0: this.id = (Id) newval; break;
		 case 1: this.formals = (Formals) newval; break;
		 case 2: this.from = (Pat) newval; break;
		 case 3: this.envelope = (Pat) newval; break;
		 case 4: this.hasPrio = (Boolean) newval; break;
		 case 5: this.prio = (Integer) newval; break;
		 case 6: this.body = (Cmd) newval; break;
		 case 7: this.checked = (Boolean) newval; break;
		 case 8: this.isMarkedPure = (Boolean) newval; break;

        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        


          public String genericName(){return "MonoBody";}
        
public Syntax internalDeepCopy(Token start, Token end)  {
Syntax copy = new MonoBody(start, end, 
(id == null ? null : (Id)id.internalDeepCopy(start,end)), 
(formals == null ? null : (Formals)formals.internalDeepCopy(start,end)), 
(from == null ? null : (Pat)from.internalDeepCopy(start,end)), 
(envelope == null ? null : (Pat)envelope.internalDeepCopy(start,end)), hasPrio, prio, 
(body == null ? null : (Cmd)body.internalDeepCopy(start,end)), checked, isMarkedPure);

return copy;
}

        public boolean isMarkedPure() {return this.isMarkedPure;}
     

     public String funname() {return id == null ? null : id.str(); }
     public Id idOfName(){return id;}
     

     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     
}