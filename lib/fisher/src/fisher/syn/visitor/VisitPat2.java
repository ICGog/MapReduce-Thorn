package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface VisitPat2<ARG1, ARG2, RET,  EXN extends Exception> {
    RET visit(Pat syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatVar syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatLiteral syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatListCtor syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatWildcard syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatInterpolation syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatEvalTestExp syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatAnd syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatOr syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatNot syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatNotNull syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatMatchSomethingElse syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatExtract syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatTypeTest syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatRecordCtor syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatRange syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatMethodCall syn, ARG1 arg1, ARG2 arg2) throws EXN;
    RET visit(PatSlash syn, ARG1 arg1, ARG2 arg2) throws EXN;
} 