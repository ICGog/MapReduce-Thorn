package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface ModuleFileMemberWalker<ARG,  EXN extends Exception> {
     void visit(Bind syn, ARG arg) throws EXN; 
     void visit(VarDecl syn, ARG arg) throws EXN; 
     void visit(FunDecl syn, ARG arg) throws EXN; 
     void visit(MethDecl syn, ARG arg) throws EXN; 
     void visit(ClsDecl syn, ARG arg) throws EXN; 
     void visit(ImportStmt syn, ARG arg) throws EXN; 
     void visit(ModuleFileMemberStmt syn, ARG arg) throws EXN; 
     void visit(ModuleFileImport syn, ARG arg) throws EXN; 
     void visit(ModuleFileVisibility syn, ARG arg) throws EXN; 
     void visit(ModuleFileAlias syn, ARG arg) throws EXN; 
     void visit(ComponentDecl syn, ARG arg) throws EXN; 
     void visit(JavalyFun syn, ARG arg) throws EXN; 
     void visit(JavalyClassDecl syn, ARG arg) throws EXN; 
} 