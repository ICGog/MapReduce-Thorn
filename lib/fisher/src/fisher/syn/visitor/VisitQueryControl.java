package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface VisitQueryControl<ARG, RET,  EXN extends Exception> {
    RET visit(QueryControl syn, ARG arg) throws EXN;
    RET visit(QueryControlFor syn, ARG arg) throws EXN;
    RET visit(QueryControlIf syn, ARG arg) throws EXN;
    RET visit(QueryControlVal syn, ARG arg) throws EXN;
    RET visit(QueryControlVar syn, ARG arg) throws EXN;
    RET visit(QueryControlWhile syn, ARG arg) throws EXN;
} 