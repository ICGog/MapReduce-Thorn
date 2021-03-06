package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface VisitCmd<ARG, RET,  EXN extends Exception> {
    RET visit(Cmd syn, ARG arg) throws EXN;
    RET visit(Literal syn, ARG arg) throws EXN;
    RET visit(StringWithInterpolations syn, ARG arg) throws EXN;
    RET visit(VarExp syn, ARG arg) throws EXN;
    RET visit(This syn, ARG arg) throws EXN;
    RET visit(Parens syn, ARG arg) throws EXN;
    RET visit(Valof syn, ARG arg) throws EXN;
    RET visit(Assign syn, ARG arg) throws EXN;
    RET visit(Bind syn, ARG arg) throws EXN;
    RET visit(VarDecl syn, ARG arg) throws EXN;
    RET visit(OpExp syn, ARG arg) throws EXN;
    RET visit(OpABExp syn, ARG arg) throws EXN;
    RET visit(TypedExp syn, ARG arg) throws EXN;
    RET visit(ListForGroup syn, ARG arg) throws EXN;
    RET visit(Comparison syn, ARG arg) throws EXN;
    RET visit(MatchExp syn, ARG arg) throws EXN;
    RET visit(ItExp syn, ARG arg) throws EXN;
    RET visit(ForPatternOnly syn, ARG arg) throws EXN;
    RET visit(WildcardExp syn, ARG arg) throws EXN;
    RET visit(DotMethodCallExp syn, ARG arg) throws EXN;
    RET visit(InterpolationExp syn, ARG arg) throws EXN;
    RET visit(EvalTestExpExp syn, ARG arg) throws EXN;
    RET visit(ExpExtract syn, ARG arg) throws EXN;
    RET visit(Probe syn, ARG arg) throws EXN;
    RET visit(ListCtor syn, ARG arg) throws EXN;
    RET visit(AnonFun syn, ARG arg) throws EXN;
    RET visit(MethodCall syn, ARG arg) throws EXN;
    RET visit(FunCall syn, ARG arg) throws EXN;
    RET visit(BracketCall syn, ARG arg) throws EXN;
    RET visit(FieldRef syn, ARG arg) throws EXN;
    RET visit(SuperThingie syn, ARG arg) throws EXN;
    RET visit(SuperCall syn, ARG arg) throws EXN;
    RET visit(SuperCtorCall syn, ARG arg) throws EXN;
    RET visit(If syn, ARG arg) throws EXN;
    RET visit(Return syn, ARG arg) throws EXN;
    RET visit(LabellableLoop syn, ARG arg) throws EXN;
    RET visit(Signature syn, ARG arg) throws EXN;
    RET visit(While syn, ARG arg) throws EXN;
    RET visit(For syn, ARG arg) throws EXN;
    RET visit(Seq syn, ARG arg) throws EXN;
    RET visit(Skip syn, ARG arg) throws EXN;
    RET visit(FunDecl syn, ARG arg) throws EXN;
    RET visit(Break syn, ARG arg) throws EXN;
    RET visit(Continue syn, ARG arg) throws EXN;
    RET visit(Match syn, ARG arg) throws EXN;
    RET visit(Try syn, ARG arg) throws EXN;
    RET visit(Throw syn, ARG arg) throws EXN;
    RET visit(RecordCtor syn, ARG arg) throws EXN;
    RET visit(RecordCall syn, ARG arg) throws EXN;
    RET visit(AnonObj syn, ARG arg) throws EXN;
    RET visit(ClassFormal syn, ARG arg) throws EXN;
    RET visit(ClsDecl syn, ARG arg) throws EXN;
    RET visit(ImportStmt syn, ARG arg) throws EXN;
    RET visit(Alias syn, ARG arg) throws EXN;
    RET visit(CmdsInAList syn, ARG arg) throws EXN;
    RET visit(AbstractTable syn, ARG arg) throws EXN;
    RET visit(Table syn, ARG arg) throws EXN;
    RET visit(Ord syn, ARG arg) throws EXN;
    RET visit(MapCtor syn, ARG arg) throws EXN;
    RET visit(QueryAbstract syn, ARG arg) throws EXN;
    RET visit(QuerySwiss syn, ARG arg) throws EXN;
    RET visit(QueryFirstlike syn, ARG arg) throws EXN;
    RET visit(QueryFirst syn, ARG arg) throws EXN;
    RET visit(QueryAfter syn, ARG arg) throws EXN;
    RET visit(QueryQuantifier syn, ARG arg) throws EXN;
    RET visit(QueryQuantifierCount syn, ARG arg) throws EXN;
    RET visit(QueryQuantifierEvery syn, ARG arg) throws EXN;
    RET visit(QueryQuantifierSome syn, ARG arg) throws EXN;
    RET visit(QueryListComprehension syn, ARG arg) throws EXN;
    RET visit(QueryTable syn, ARG arg) throws EXN;
    RET visit(QueryGroup syn, ARG arg) throws EXN;
    RET visit(QuerySort syn, ARG arg) throws EXN;
    RET visit(Spawn syn, ARG arg) throws EXN;
    RET visit(SpawnByComponentName syn, ARG arg) throws EXN;
    RET visit(AsyncStmt syn, ARG arg) throws EXN;
    RET visit(SyncStmt syn, ARG arg) throws EXN;
    RET visit(Send syn, ARG arg) throws EXN;
    RET visit(Recv syn, ARG arg) throws EXN;
    RET visit(Serve syn, ARG arg) throws EXN;
    RET visit(ComponentDecl syn, ARG arg) throws EXN;
    RET visit(Javaly syn, ARG arg) throws EXN;
    RET visit(JavalyFun syn, ARG arg) throws EXN;
    RET visit(JavalyNewDecl syn, ARG arg) throws EXN;
    RET visit(JavalyMethodDecl syn, ARG arg) throws EXN;
    RET visit(JavalyClassDecl syn, ARG arg) throws EXN;
} 