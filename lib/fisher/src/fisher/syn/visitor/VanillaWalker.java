package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.interfaces.*;
import fisher.syn.core.*;
public abstract class VanillaWalker<ARG,  EXN extends Exception>
   implements Walker<ARG,EXN>
{
    public abstract void visit(Syntax syn, ARG arg) throws EXN;
    public void visit(Id syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Cmd syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Pat syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(QualName syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Formals syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(TypeConstraint syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(TypeConstraints syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Literal syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(AbstractStringBit syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(StringBitText syn, ARG arg) throws EXN {
        this.visit((AbstractStringBit)syn, arg);
       }
    public void visit(StringBitVar syn, ARG arg) throws EXN {
        this.visit((AbstractStringBit)syn, arg);
       }
    public void visit(StringWithInterpolations syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(VarExp syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(This syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Parens syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Valof syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(AssignTarget syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(AssignToId syn, ARG arg) throws EXN {
        this.visit((AssignTarget)syn, arg);
       }
    public void visit(AssignTofield syn, ARG arg) throws EXN {
        this.visit((AssignTarget)syn, arg);
       }
    public void visit(AssignToSubscripted syn, ARG arg) throws EXN {
        this.visit((AssignTarget)syn, arg);
       }
    public void visit(AssignToMap syn, ARG arg) throws EXN {
        this.visit((AssignTarget)syn, arg);
       }
    public void visit(AssignToFieldOfSubscripted syn, ARG arg) throws EXN {
        this.visit((AssignTarget)syn, arg);
       }
    public void visit(Assign syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Bind syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(VarDecl syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(OpExp syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(OpABExp syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(TypedExp syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ListForGroup syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ComparisonBit syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Comparison syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(MatchExp syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ItExp syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ForPatternOnly syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(WildcardExp syn, ARG arg) throws EXN {
        this.visit((ForPatternOnly)syn, arg);
       }
    public void visit(DotMethodCallExp syn, ARG arg) throws EXN {
        this.visit((ForPatternOnly)syn, arg);
       }
    public void visit(InterpolationExp syn, ARG arg) throws EXN {
        this.visit((ForPatternOnly)syn, arg);
       }
    public void visit(EvalTestExpExp syn, ARG arg) throws EXN {
        this.visit((ForPatternOnly)syn, arg);
       }
    public void visit(ExpExtract syn, ARG arg) throws EXN {
        this.visit((ForPatternOnly)syn, arg);
       }
    public void visit(Probe syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ListBit syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ListBitExp syn, ARG arg) throws EXN {
        this.visit((ListBit)syn, arg);
       }
    public void visit(ListBitEllip syn, ARG arg) throws EXN {
        this.visit((ListBit)syn, arg);
       }
    public void visit(ListCtor syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(MonoBody syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(FunBody syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(AnonFun syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(MethodCall syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(FunCall syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(BracketCall syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(FieldRef syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(SuperThingie syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(SuperCall syn, ARG arg) throws EXN {
        this.visit((SuperThingie)syn, arg);
       }
    public void visit(SuperCtorCall syn, ARG arg) throws EXN {
        this.visit((SuperThingie)syn, arg);
       }
    public void visit(PostExp syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(PostExpDotId syn, ARG arg) throws EXN {
        this.visit((PostExp)syn, arg);
       }
    public void visit(PostExpArgs syn, ARG arg) throws EXN {
        this.visit((PostExp)syn, arg);
       }
    public void visit(PostExpBracketArgs syn, ARG arg) throws EXN {
        this.visit((PostExp)syn, arg);
       }
    public void visit(If syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Return syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(LabellableLoop syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Signature syn, ARG arg) throws EXN {
        this.visit((LabellableLoop)syn, arg);
       }
    public void visit(While syn, ARG arg) throws EXN {
        this.visit((LabellableLoop)syn, arg);
       }
    public void visit(For syn, ARG arg) throws EXN {
        this.visit((LabellableLoop)syn, arg);
       }
    public void visit(Seq syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Skip syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(FunDecl syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Break syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Continue syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Case syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Match syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Try syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Throw syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(PatVar syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatLiteral syn, ARG arg) throws EXN {
        this.visit((Pat)syn, arg);
       }
    public void visit(PatListBit syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(PatListBitExp syn, ARG arg) throws EXN {
        this.visit((PatListBit)syn, arg);
       }
    public void visit(PatListBitEllip syn, ARG arg) throws EXN {
        this.visit((PatListBit)syn, arg);
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
    public void visit(PatRecordField syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
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
    public void visit(RecordField syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(RecordCtor syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(RecordCall syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(PostExpRecordArgs syn, ARG arg) throws EXN {
        this.visit((PostExp)syn, arg);
       }
    public void visit(MethDecl syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ClsCtorDef syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ClsPatDef syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ClsExtends syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(AnonObj syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ClassFormal syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ClsDecl syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ModArgBinding syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ImportStmt syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Alias syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ModuleFileMemberStmt syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ModuleFileImport syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ModuleFileVisibility syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ModuleFileAlias syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Module syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ModulesInAList syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(CmdsInAList syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(SyntaxInAList syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(IdWithOptInit syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(TableFields syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(AbstractTable syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(TableKey syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Table syn, ARG arg) throws EXN {
        this.visit((AbstractTable)syn, arg);
       }
    public void visit(Ord syn, ARG arg) throws EXN {
        this.visit((AbstractTable)syn, arg);
       }
    public void visit(MapCtor syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(QueryControl syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(QueryControlFor syn, ARG arg) throws EXN {
        this.visit((QueryControl)syn, arg);
       }
    public void visit(QueryControlIf syn, ARG arg) throws EXN {
        this.visit((QueryControl)syn, arg);
       }
    public void visit(QueryControlVal syn, ARG arg) throws EXN {
        this.visit((QueryControl)syn, arg);
       }
    public void visit(QueryControlVar syn, ARG arg) throws EXN {
        this.visit((QueryControl)syn, arg);
       }
    public void visit(QueryControlWhile syn, ARG arg) throws EXN {
        this.visit((QueryControl)syn, arg);
       }
    public void visit(QueryAbstract syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(QuerySwiss syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(QueryFirstlike syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(QueryFirst syn, ARG arg) throws EXN {
        this.visit((QueryFirstlike)syn, arg);
       }
    public void visit(QueryAfter syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(QueryQuantifier syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(QueryQuantifierCount syn, ARG arg) throws EXN {
        this.visit((QueryQuantifier)syn, arg);
       }
    public void visit(QueryQuantifierEvery syn, ARG arg) throws EXN {
        this.visit((QueryQuantifier)syn, arg);
       }
    public void visit(QueryQuantifierSome syn, ARG arg) throws EXN {
        this.visit((QueryQuantifier)syn, arg);
       }
    public void visit(QueryListComprehension syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(QueryTable syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(QGKey syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(QGAccum syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(QueryGroup syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(SortKey syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(QuerySort syn, ARG arg) throws EXN {
        this.visit((QueryAbstract)syn, arg);
       }
    public void visit(HighLevelCommunication syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(SyncDecl syn, ARG arg) throws EXN {
        this.visit((HighLevelCommunication)syn, arg);
       }
    public void visit(AsyncDecl syn, ARG arg) throws EXN {
        this.visit((HighLevelCommunication)syn, arg);
       }
    public void visit(ProcInit syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(ProcBody syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Spawn syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(SpawnByComponentName syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(AsyncStmt syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(SyncStmt syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Send syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Recv syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ServeBlock syn, ARG arg) throws EXN {
        this.visit((Syntax)syn, arg);
       }
    public void visit(Serve syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(ComponentDecl syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(Javaly syn, ARG arg) throws EXN {
        this.visit((Cmd)syn, arg);
       }
    public void visit(JavalyFun syn, ARG arg) throws EXN {
        this.visit((Javaly)syn, arg);
       }
    public void visit(JavalyNewDecl syn, ARG arg) throws EXN {
        this.visit((Javaly)syn, arg);
       }
    public void visit(JavalyMethodDecl syn, ARG arg) throws EXN {
        this.visit((Javaly)syn, arg);
       }
    public void visit(JavalyClassDecl syn, ARG arg) throws EXN {
        this.visit((Javaly)syn, arg);
       }
} 