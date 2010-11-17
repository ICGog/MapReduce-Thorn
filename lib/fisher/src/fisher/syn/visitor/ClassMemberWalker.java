package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface ClassMemberWalker<ARG,  EXN extends Exception> {
     void visit(Bind syn, ARG arg) throws EXN; 
     void visit(VarDecl syn, ARG arg) throws EXN; 
     void visit(MethDecl syn, ARG arg) throws EXN; 
     void visit(ClsCtorDef syn, ARG arg) throws EXN; 
     void visit(ClsPatDef syn, ARG arg) throws EXN; 
     void visit(ImportStmt syn, ARG arg) throws EXN; 
} 