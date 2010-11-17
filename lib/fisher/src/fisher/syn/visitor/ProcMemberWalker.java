package fisher.syn.visitor;
/* Beware!  This is a generated file.  Change the generator's dataset! */
import fisher.syn.*;
import fisher.syn.core.*;
import fisher.syn.interfaces.*;
public interface ProcMemberWalker<ARG,  EXN extends Exception> {
     void visit(Bind syn, ARG arg) throws EXN; 
     void visit(VarDecl syn, ARG arg) throws EXN; 
     void visit(FunDecl syn, ARG arg) throws EXN; 
     void visit(MethDecl syn, ARG arg) throws EXN; 
     void visit(ClsDecl syn, ARG arg) throws EXN; 
     void visit(ImportStmt syn, ARG arg) throws EXN; 
     void visit(HighLevelCommunication syn, ARG arg) throws EXN; 
     void visit(SyncDecl syn, ARG arg) throws EXN; 
     void visit(AsyncDecl syn, ARG arg) throws EXN; 
     void visit(ProcInit syn, ARG arg) throws EXN; 
     void visit(ProcBody syn, ARG arg) throws EXN; 
     void visit(Spawn syn, ARG arg) throws EXN; 
     void visit(SpawnByComponentName syn, ARG arg) throws EXN; 
     void visit(AsyncStmt syn, ARG arg) throws EXN; 
     void visit(SyncStmt syn, ARG arg) throws EXN; 
     void visit(Send syn, ARG arg) throws EXN; 
     void visit(Recv syn, ARG arg) throws EXN; 
     void visit(ServeBlock syn, ARG arg) throws EXN; 
     void visit(Serve syn, ARG arg) throws EXN; 
     void visit(ComponentDecl syn, ARG arg) throws EXN; 
     void visit(Javaly syn, ARG arg) throws EXN; 
     void visit(JavalyFun syn, ARG arg) throws EXN; 
     void visit(JavalyNewDecl syn, ARG arg) throws EXN; 
     void visit(JavalyMethodDecl syn, ARG arg) throws EXN; 
     void visit(JavalyClassDecl syn, ARG arg) throws EXN; 
} 