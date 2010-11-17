package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface ModuleFileMemberVisitor<ARG, RET,  EXN extends Exception> {
     RET visit(Bind syn, ARG arg) throws EXN; 
     RET visit(VarDecl syn, ARG arg) throws EXN; 
     RET visit(FunDecl syn, ARG arg) throws EXN; 
     RET visit(MethDecl syn, ARG arg) throws EXN; 
     RET visit(ClsDecl syn, ARG arg) throws EXN; 
     RET visit(ImportStmt syn, ARG arg) throws EXN; 
     RET visit(ModuleFileMemberStmt syn, ARG arg) throws EXN; 
     RET visit(ModuleFileImport syn, ARG arg) throws EXN; 
     RET visit(ModuleFileVisibility syn, ARG arg) throws EXN; 
     RET visit(ModuleFileAlias syn, ARG arg) throws EXN; 
     RET visit(ComponentDecl syn, ARG arg) throws EXN; 
     RET visit(JavalyFun syn, ARG arg) throws EXN; 
     RET visit(JavalyClassDecl syn, ARG arg) throws EXN; 
} 