package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.interfaces.*;
import fisher.syn.core.*;
public abstract class VanillaVisitor<ARG, RET,  EXN extends Exception>
   implements Visitor<ARG,RET, EXN>
{
    public abstract RET visit(Syntax syn, ARG arg) throws EXN;
    public RET visit(Id syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Cmd syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Pat syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(QualName syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Formals syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(TypeConstraint syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(TypeConstraints syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Literal syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(AbstractStringBit syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(StringBitText syn, ARG arg) throws EXN {
       return this.visit((AbstractStringBit)syn, arg);
       }
    public RET visit(StringBitVar syn, ARG arg) throws EXN {
       return this.visit((AbstractStringBit)syn, arg);
       }
    public RET visit(StringWithInterpolations syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(VarExp syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(This syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Parens syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Valof syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(AssignTarget syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(AssignToId syn, ARG arg) throws EXN {
       return this.visit((AssignTarget)syn, arg);
       }
    public RET visit(AssignTofield syn, ARG arg) throws EXN {
       return this.visit((AssignTarget)syn, arg);
       }
    public RET visit(AssignToSubscripted syn, ARG arg) throws EXN {
       return this.visit((AssignTarget)syn, arg);
       }
    public RET visit(AssignToMap syn, ARG arg) throws EXN {
       return this.visit((AssignTarget)syn, arg);
       }
    public RET visit(AssignToFieldOfSubscripted syn, ARG arg) throws EXN {
       return this.visit((AssignTarget)syn, arg);
       }
    public RET visit(Assign syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Bind syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(VarDecl syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(OpExp syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(OpABExp syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(TypedExp syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ListForGroup syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ComparisonBit syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Comparison syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(MatchExp syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ItExp syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ForPatternOnly syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(WildcardExp syn, ARG arg) throws EXN {
       return this.visit((ForPatternOnly)syn, arg);
       }
    public RET visit(DotMethodCallExp syn, ARG arg) throws EXN {
       return this.visit((ForPatternOnly)syn, arg);
       }
    public RET visit(InterpolationExp syn, ARG arg) throws EXN {
       return this.visit((ForPatternOnly)syn, arg);
       }
    public RET visit(EvalTestExpExp syn, ARG arg) throws EXN {
       return this.visit((ForPatternOnly)syn, arg);
       }
    public RET visit(ExpExtract syn, ARG arg) throws EXN {
       return this.visit((ForPatternOnly)syn, arg);
       }
    public RET visit(Probe syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ListBit syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ListBitExp syn, ARG arg) throws EXN {
       return this.visit((ListBit)syn, arg);
       }
    public RET visit(ListBitEllip syn, ARG arg) throws EXN {
       return this.visit((ListBit)syn, arg);
       }
    public RET visit(ListCtor syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(MonoBody syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(FunBody syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(AnonFun syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(MethodCall syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(FunCall syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(BracketCall syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(FieldRef syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(SuperThingie syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(SuperCall syn, ARG arg) throws EXN {
       return this.visit((SuperThingie)syn, arg);
       }
    public RET visit(SuperCtorCall syn, ARG arg) throws EXN {
       return this.visit((SuperThingie)syn, arg);
       }
    public RET visit(PostExp syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(PostExpDotId syn, ARG arg) throws EXN {
       return this.visit((PostExp)syn, arg);
       }
    public RET visit(PostExpArgs syn, ARG arg) throws EXN {
       return this.visit((PostExp)syn, arg);
       }
    public RET visit(PostExpBracketArgs syn, ARG arg) throws EXN {
       return this.visit((PostExp)syn, arg);
       }
    public RET visit(If syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Return syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(LabellableLoop syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Signature syn, ARG arg) throws EXN {
       return this.visit((LabellableLoop)syn, arg);
       }
    public RET visit(While syn, ARG arg) throws EXN {
       return this.visit((LabellableLoop)syn, arg);
       }
    public RET visit(For syn, ARG arg) throws EXN {
       return this.visit((LabellableLoop)syn, arg);
       }
    public RET visit(Seq syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Skip syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(FunDecl syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Break syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Continue syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Case syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Match syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Try syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Throw syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(PatVar syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatLiteral syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatListBit syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(PatListBitExp syn, ARG arg) throws EXN {
       return this.visit((PatListBit)syn, arg);
       }
    public RET visit(PatListBitEllip syn, ARG arg) throws EXN {
       return this.visit((PatListBit)syn, arg);
       }
    public RET visit(PatListCtor syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatWildcard syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatInterpolation syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatEvalTestExp syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatAnd syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatOr syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatNot syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatNotNull syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatMatchSomethingElse syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatExtract syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatTypeTest syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatRecordField syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(PatRecordCtor syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatRange syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatMethodCall syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(PatSlash syn, ARG arg) throws EXN {
       return this.visit((Pat)syn, arg);
       }
    public RET visit(RecordField syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(RecordCtor syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(RecordCall syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(PostExpRecordArgs syn, ARG arg) throws EXN {
       return this.visit((PostExp)syn, arg);
       }
    public RET visit(MethDecl syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ClsCtorDef syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ClsPatDef syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ClsExtends syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(AnonObj syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ClassFormal syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ClsDecl syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ModArgBinding syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ImportStmt syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Alias syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ModuleFileMemberStmt syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ModuleFileImport syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ModuleFileVisibility syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ModuleFileAlias syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Module syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ModulesInAList syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(CmdsInAList syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(SyntaxInAList syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(IdWithOptInit syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(TableFields syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(AbstractTable syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(TableKey syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Table syn, ARG arg) throws EXN {
       return this.visit((AbstractTable)syn, arg);
       }
    public RET visit(Ord syn, ARG arg) throws EXN {
       return this.visit((AbstractTable)syn, arg);
       }
    public RET visit(MapCtor syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(QueryControl syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(QueryControlFor syn, ARG arg) throws EXN {
       return this.visit((QueryControl)syn, arg);
       }
    public RET visit(QueryControlIf syn, ARG arg) throws EXN {
       return this.visit((QueryControl)syn, arg);
       }
    public RET visit(QueryControlVal syn, ARG arg) throws EXN {
       return this.visit((QueryControl)syn, arg);
       }
    public RET visit(QueryControlVar syn, ARG arg) throws EXN {
       return this.visit((QueryControl)syn, arg);
       }
    public RET visit(QueryControlWhile syn, ARG arg) throws EXN {
       return this.visit((QueryControl)syn, arg);
       }
    public RET visit(QueryAbstract syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(QuerySwiss syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(QueryFirstlike syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(QueryFirst syn, ARG arg) throws EXN {
       return this.visit((QueryFirstlike)syn, arg);
       }
    public RET visit(QueryAfter syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(QueryQuantifier syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(QueryQuantifierCount syn, ARG arg) throws EXN {
       return this.visit((QueryQuantifier)syn, arg);
       }
    public RET visit(QueryQuantifierEvery syn, ARG arg) throws EXN {
       return this.visit((QueryQuantifier)syn, arg);
       }
    public RET visit(QueryQuantifierSome syn, ARG arg) throws EXN {
       return this.visit((QueryQuantifier)syn, arg);
       }
    public RET visit(QueryListComprehension syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(QueryTable syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(QGKey syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(QGAccum syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(QueryGroup syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(SortKey syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(QuerySort syn, ARG arg) throws EXN {
       return this.visit((QueryAbstract)syn, arg);
       }
    public RET visit(HighLevelCommunication syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(SyncDecl syn, ARG arg) throws EXN {
       return this.visit((HighLevelCommunication)syn, arg);
       }
    public RET visit(AsyncDecl syn, ARG arg) throws EXN {
       return this.visit((HighLevelCommunication)syn, arg);
       }
    public RET visit(ProcInit syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(ProcBody syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Spawn syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(SpawnByComponentName syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(AsyncStmt syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(SyncStmt syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Send syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Recv syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ServeBlock syn, ARG arg) throws EXN {
       return this.visit((Syntax)syn, arg);
       }
    public RET visit(Serve syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(ComponentDecl syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(Javaly syn, ARG arg) throws EXN {
       return this.visit((Cmd)syn, arg);
       }
    public RET visit(JavalyFun syn, ARG arg) throws EXN {
       return this.visit((Javaly)syn, arg);
       }
    public RET visit(JavalyNewDecl syn, ARG arg) throws EXN {
       return this.visit((Javaly)syn, arg);
       }
    public RET visit(JavalyMethodDecl syn, ARG arg) throws EXN {
       return this.visit((Javaly)syn, arg);
       }
    public RET visit(JavalyClassDecl syn, ARG arg) throws EXN {
       return this.visit((Javaly)syn, arg);
       }
} 