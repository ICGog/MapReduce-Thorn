package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface VisitPat<ARG, RET,  EXN extends Exception> {
    RET visit(Pat syn, ARG arg) throws EXN;
    RET visit(PatVar syn, ARG arg) throws EXN;
    RET visit(PatLiteral syn, ARG arg) throws EXN;
    RET visit(PatListCtor syn, ARG arg) throws EXN;
    RET visit(PatWildcard syn, ARG arg) throws EXN;
    RET visit(PatInterpolation syn, ARG arg) throws EXN;
    RET visit(PatEvalTestExp syn, ARG arg) throws EXN;
    RET visit(PatAnd syn, ARG arg) throws EXN;
    RET visit(PatOr syn, ARG arg) throws EXN;
    RET visit(PatNot syn, ARG arg) throws EXN;
    RET visit(PatNotNull syn, ARG arg) throws EXN;
    RET visit(PatMatchSomethingElse syn, ARG arg) throws EXN;
    RET visit(PatExtract syn, ARG arg) throws EXN;
    RET visit(PatTypeTest syn, ARG arg) throws EXN;
    RET visit(PatRecordCtor syn, ARG arg) throws EXN;
    RET visit(PatRange syn, ARG arg) throws EXN;
    RET visit(PatMethodCall syn, ARG arg) throws EXN;
    RET visit(PatSlash syn, ARG arg) throws EXN;
} 