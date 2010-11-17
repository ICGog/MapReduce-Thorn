package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface VisitQueryAbstract<ARG, RET,  EXN extends Exception> {
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
} 