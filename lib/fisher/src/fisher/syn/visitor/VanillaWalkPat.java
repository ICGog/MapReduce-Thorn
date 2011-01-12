package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.interfaces.*;
import fisher.syn.core.*;
public abstract class VanillaWalkPat<ARG,  EXN extends Exception>
   implements WalkPat<ARG,EXN>
{
    public abstract void visit(Pat syn, ARG arg) throws EXN;
    public void visit(PatVar syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatLiteral syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatListCtor syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatWildcard syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatInterpolation syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatEvalTestExp syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatAnd syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatOr syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatNot syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatNotNull syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatMatchSomethingElse syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatExtract syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatTypeTest syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatRecordCtor syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatRange syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatMethodCall syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatSlash syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
} 