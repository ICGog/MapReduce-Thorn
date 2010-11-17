
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.desugar;

import java.util.Collections;
import java.util.List;

import fisher.ingest.Ingester;
import fisher.parser.Token;
import fisher.runtime.builtInFun.LastLetterBIF;
import fisher.runtime.builtInFun.SyncReplyBIF;
import fisher.syn.AnonFun;
import fisher.syn.AsyncDecl;
import fisher.syn.Bind;
import fisher.syn.Case;
import fisher.syn.Cmd;
import fisher.syn.Formals;
import fisher.syn.FunBody;
import fisher.syn.FunCall;
import fisher.syn.FunDecl;
import fisher.syn.HighLevelCommunication;
import fisher.syn.If;
import fisher.syn.MonoBody;
import fisher.syn.Pat;
import fisher.syn.PatRecordCtor;
import fisher.syn.PatVar;
import fisher.syn.RecordCtor;
import fisher.syn.Recv;
import fisher.syn.Send;
import fisher.syn.Seq;
import fisher.syn.Serve;
import fisher.syn.ServeBlock;
import fisher.syn.SyncDecl;
import fisher.syn.VarDecl;
import fisher.syn.core.Id;
import fisher.syn.core.Op;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ComponentInfo;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  DistDesugarer extends AbstractDesugarer  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	final public static String async = "async";
	final public static String sync = "sync";
	final public static String args = "args";
	final public static String nonce = "nonce";
	final public static String response = "response";
	final public static String msg = "msg";
	final public static String sender = "sender";
	final public static String envelope = "envelope";

	public DistDesugarer(Token start, Token end) {
		super(start, end);
		// TODO Auto-generated constructor stub
	}

	public static Cmd serve(Serve serve) {
		DistDesugarer dd = new DistDesugarer(serve.start, serve.end);
		return dd.doServe(serve);
	}

	public static Cmd asyncCall(Token start, Token end, Cmd receiver, Id name, List<Cmd> actuals, Cmd tokens) {
		DistDesugarer dd = new DistDesugarer(start, end);
		return dd.asyncCall(receiver, name, actuals, tokens);
	}

	public static Cmd syncCall(Token start, Token end, Cmd receiver, Id name, List<Cmd> actuals, Cmd timN, Cmd timCmd, Cmd tokens) {
		DistDesugarer dd = new DistDesugarer(start, end);
		return dd.syncCall(receiver, name, actuals, timN, timCmd, tokens);
	}

	public static void addSpawnuments(ComponentInfo src) {
		Syntax synsrc = ((Syntax) src);
		DistDesugarer dd = new DistDesugarer(synsrc.start, synsrc.end);
		dd.addSpawnumentsTo(src);
	}

	private Cmd asyncCall(Cmd receiver, Id name, List<Cmd> actuals, Cmd tokens) {
		// rcvr <-- whack(n); 
		// desugars to: 
		// rcvr <<< {: `async`:"whack" , args: [n] :};

		//		System.err.println("DD.async < " + receiver + " <-- " + name + "(" + Bard.sep(actuals, ", ") + ")");
		Id iasync = id(async);
		Cmd casync = lit(name.str());
		Id iargs = id(args);
		Cmd cargs = listctor(actuals);

		Cmd packet = rec(iasync, casync, iargs, cargs);
		Send send = send(cmd(receiver), packet, cmd(tokens));
		//		System.err.println("DD.async > " + send);

		return send;
	}

	private Cmd syncCall(Cmd receiver, Id name, List<Cmd> actuals, Cmd timN, Cmd timCmd, Cmd tokens) {
		// BEFORE:
		//   rcvr <-> trudge(n) timeout(timN) {timCmd();}
		// 	
		// AFTER: 
		//a   {
		//b	    CnonceC = newNonce();
		//c     CreceiverC = receiver; 
		//d	    CreceiverC <<< {: `sync`: "trudge", args: [n], nonce: CnonceC :};
		//e	    recv{ 
		//f	       {: response: CxC, nonce: $(CnonceC) :} from $(CreceiverC) => CxC
		//g	       timeout (timN) { timCmd(); }
		//h	  };

		// line b
		Id CnonceC = DesugarUtils.gensym(start, "nonce");
		Bind b = bind(CnonceC, funcall(varexp(id("newNonce"))));

		// line c
		Id CreceiverC = DesugarUtils.gensym(start, "receiver");
		Bind c = bind(CreceiverC, cmd(receiver));

		// line d
		Id isync = id(sync);
		Cmd csync = lit(name.str());
		Id inonce = id(nonce);
		Id iargs = id(args);
		Cmd cargs = listctor(actuals);

		RecordCtor packet = rec(isync, csync, inonce, varexp(CnonceC), iargs, cargs);
		Cmd d = send(varexp(CreceiverC), packet, cmd(tokens));

		// line f
		Id CxC = DesugarUtils.gensym(start, "x");
		Id iresponse = id(response);
		Pat pCxC = patvar(CxC);
		Id inonce2 = id(nonce); // a different copy
		Pat pnonce = patinterp(varexp(CnonceC));
		Pat responsePacketPat = patrec(iresponse, patvar(CxC), inonce2, pnonce);

		Pat fromReceiverPat = patwild();
		// WAS: patinterp(varexp(CreceiverC));
		Pat envelReceiverPat = patwild();
		Case f = cas(responsePacketPat, fromReceiverPat, envelReceiverPat, false, 0, varexp(CxC));
		Recv efgh = recv(timN, timCmd, f);

		Seq syncCall = seq(b, c, d, efgh);
		//	System.err.println("DD.sync  < " + receiver + "<--" + name + "(" + actuals + ") timeout(" + timN + ") " + timCmd );
		//	System.err.println("DD.sync  > " + syncCall);
		return syncCall;
	}

	private void addSpawnumentsTo(ComponentInfo src) {
		addFunsForSyncs(src);
		addFunsForAsyncs(src);
		addServe(src);
	}

	private void addFunsForSyncs(ComponentInfo src) {
		// each sync, before: 
		// sync f(a,b) from pf prio pr = body ≈
		// 
		// after: 
		//a fun CfC(sender, msg, nonce, [a,b]) =
		//b   {: response: body, nonce: nonce :}

		for (fisher.syn.HighLevelCommunication hlc : src.highLevelCommunications()) {
			if (hlc instanceof SyncDecl) {
				// hlc =    sync normal_sync(a,b) from FROM envelope ENVE prio 1 = "normal_sync_body";
				// ---->
				// fun Cs_normal_sync(FROM,  nonce, [a,b], ENVE) = "normal_sync_body";
				SyncDecl sd = (SyncDecl) hlc;
				Id CfC = DesugarUtils.gensym(sd.start, "s_" + sd.name.str());
				sd.gennedFunName = CfC;
				FunBody sdfunbody = sd.funbody;
				MonoBody sdmonobody = sdfunbody.funbodies.get(0);
				assert (sdfunbody.funbodies.size() == 1);
				Pat FROM = sdmonobody.from == null ? patwild() : pat(sdmonobody.from);
				Pat argspat = patlist(sdmonobody.formals.formals);
				Id inonce = id(nonce);
				Pat ENVE = sdmonobody.envelope == null ? patwild() : pat(sdmonobody.envelope);
				Cmd funbody = sdmonobody.body;
				MonoBody funmonobody = monobody(CfC, formals(FROM, patvar(inonce), argspat, ENVE), null, null, false,
						0, funbody, false);
				FunDecl funDecl = fundecl(CfC, funmonobody);
				if (TestUtils.desugarServe) {
								System.err.println("sync_was##     < " + hlc);
								System.err.println("sync_becomes## > " + funDecl);
				}
				src.funs().add(funDecl);
				((Syntax) src).children().add(funDecl);
			}
		}
	}

	private void addFunsForAsyncs(ComponentInfo src) {
		// before: 
		//    async an_async(e,f) from FROM envelope ENVE prio 3 = "normal_async_body";
		// 
		// after: 
		// fun Ca_async(FROM, [e,f], ENVE) {  "normal_sync_body";  }
		for (HighLevelCommunication hlc : src.highLevelCommunications()) {
			if (hlc instanceof AsyncDecl) {
				AsyncDecl ad = (AsyncDecl) hlc;
				Id CfC = DesugarUtils.gensym(ad.start, "a_" + ad.name.str());
				ad.gennedFunName = CfC;
				FunBody sdfunbody = ad.funbody;
				MonoBody sdmonobody = sdfunbody.funbodies.get(0);
				assert (sdfunbody.funbodies.size() == 1);
				Pat FROM = sdmonobody.from == null ? patwild() : pat(sdmonobody.from);
				Pat argspat = patlist(sdmonobody.formals.formals);
				Pat ENVE = sdmonobody.envelope == null ? patwild() : pat(sdmonobody.envelope);
				MonoBody funmonobody = monobody(CfC, formals(FROM, argspat, ENVE), null, null, false, 0,
						cmd(sdmonobody.body), false);
				FunDecl funDecl = fundecl(CfC, funmonobody);
				if (TestUtils.desugarServe) {
					System.err.println("async## < " + hlc);
					System.err.println("async## > " + funDecl);
				}
				src.funs().add(funDecl);
				((Syntax) src).children().add(funDecl);
				//				System.err.println("async_was##     < " + hlc);
				//				System.err.println("async_becomes## > " + funDecl);
			}
		}
	}

	private void addServe(ComponentInfo src) {
		// (This function is generated for each component.  It may never be called.)
		// inputs: 
		//   sync normal_sync(a,b) from FROM envelope ENVE prio 1 = "normal_sync_body";
		//   async an_async(e,f) from FROM envelope ENVE prio 3 = "normal_async_body";
		//
		// outputs: (Note one clause of the recv for each sync and async
		//		   fun Cserve(Cbefore, Cafter, CtimeoutN, CtimeoutCmd) {
		//a			     CsplitSync = splitSync();
		//			     recv {
		//			     {: `sync`:"normal_sync", args: Cargs, nonce: Cnonce:}
		//			     from Csender
		//			     envelope Cenvelope 
		//			     prio 1
		//			     => {
		//			          var Cresult := null;
		//			          if(Cbefore != null) Cbefore();
		//			          try {
		//			            Cresult := +(s_normal_sync(Csender, Cnonce, Cargs, Cenvelope)); // note plussing.
		//			          }
		//			          catch {
		//			            $(CsplitSync) => {}
		//			          }
		//			          finally {
		//			            if(Cafter != null) Cafter();
		//			          }
		//			          if (Cresult ~ +CcallSucceeded) {
		//			             syncReply(Cenvelope, CcallSucceeded);
		//			          }
		//			        }
		//			     |
		//			     {:`async`:"an_async", args: Cargs :}
		//			     from Csender
		//			     envelope Cenvelope
		//			     prio 3
		//			     => {
		//			          if(Cbefore != null) Cbefore();
		//			          try {
		//			            (s_normal_sync(Csender, Cargs, Cenvelope)); 
		//			          }
		//			          finally {
		//			            if(Cafter != null) Cafter();
		//			          }
		//			        }
		//			     
		//			     timeout(CtimeoutN) {
		//			       if (CtimeoutCmd != null) CtimeoutCmd();
		//			       }
		//			     }recv
		//			   }

		Id serveId = src.serveId();

		// Formals to the serve function
		Id before = DesugarUtils.gensym(start, "before");
		Id after = DesugarUtils.gensym(start, "after");
		Id timeoutN = DesugarUtils.gensym(start, "timeoutN");
		Id timeoutCmd = DesugarUtils.gensym(start, "timeoutCmd"); // a nullary function.

		Id CsplitSync = DesugarUtils.gensym(start, "splitSync");
		Cmd a = bind(CsplitSync, funcall(varexp(id("splitSync"))));

		List<Case> cases = Bard.list();
		for (HighLevelCommunication hlc : src.highLevelCommunications()) {
			Case hlcCase = caseFor(src, hlc, before, after, CsplitSync);
			cases.add(hlcCase);
		}

		Cmd evalTimeoutCmd = funcall(varexp(timeoutCmd));
		// Guard them: timeoutN may be null, timeoutCmd may be null.
		Cmd timeoutNG = varexp(timeoutN);

		Cmd evalTimeoutCmdG = ifNull(varexp(timeoutCmd), lit(null), funcall(varexp(timeoutCmd)));
		Recv recv = new Recv(start, end, cases, (timeoutNG), seq(evalTimeoutCmdG));
		try {
			Ingester.check(recv);
		} catch (FisherException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		Formals serveFormals = formals(patvar(before), patvar(after), patvar(timeoutN), patvar(timeoutCmd));
		FunDecl serveFunSyn = fundecl(serveId, monobody(serveId, serveFormals, null, null, false, 0, seq(a, recv), false));

		if (TestUtils.desugarServe) {
			System.err.println("serve = " + serveFunSyn);
		}

		src.funs().add(serveFunSyn);
		((Syntax) src).children().add(serveFunSyn);
	}

	private Case caseFor(ComponentInfo src, fisher.syn.HighLevelCommunication hlc, Id before, Id after, Id CsplitSync) {
		// inputs: 
		//   sync normal_sync(a,b) from FROM envelope ENVE prio 1 = "normal_sync_body";
		// OUTPUTS: 
		//	     {: `sync`:"normal_sync", args: Cargs, nonce: Cnonce:}
		//	     from Csender
		//	     envelope Cenvelope 
		//	     prio 1
		//	     => {
		//	          var Cresult := null;
		//	          if(Cbefore != null) Cbefore();
		//	          try {
		//	            Cresult := +(s_normal_sync(Csender, Cnonce, Cargs, Cenvelope)); // note plussing.
		//	          }
		//	          catch {
		//	            $(CsplitSync) => {}
		//	          }
		//	          finally {
		//	            if(Cafter != null) Cafter();
		//	          }
		//	          if (Cresult ~ +CcallSucceeded) {
		//	             syncReply(Cenvelope, CcallSucceeded);
		//	          }
		//	        }

		Id args = DesugarUtils.gensym(start, "args");
		Id nonce = DesugarUtils.gensym(start, DistDesugarer.nonce);
		Id senderId = DesugarUtils.gensym(start, sender);
		Id envelopeId = DesugarUtils.gensym(start, "envelope");
		Pat requestPacket = commRecord(hlc, args, nonce);
		MonoBody monoBody = hlc.funbody.funbodies.get(0);
		Pat senderPat = monoBody.from == null ? varpat(senderId) : patand(varpat(senderId), pat(monoBody.from));
		Pat envelPat = monoBody.envelope == null ? varpat(envelopeId) : patand(varpat(envelopeId),
				pat(monoBody.envelope));
		Cmd responseToRequestBody = seq(commReaction(src, hlc, senderId, envelopeId, before, after, args, nonce,
				CsplitSync));
		return cas(requestPacket, senderPat, envelPat, monoBody.hasPrio, monoBody.prio, responseToRequestBody);
	}

	private Cmd commReaction(ComponentInfo src, HighLevelCommunication hlc, Id senderId, Id envelopeId, Id before,
			Id after, Id args, Id nonce, Id CsplitSync) {
		// inputs: 
		//   sync normal_sync(a,b) from FROM envelope ENVE prio 1 = "normal_sync_body";
		// OUTPUTS:  depends on whether the hlc is a sync or async.

		Cmd doBefore = ifNull(varexp(before), seq(), funcall(varexp(before), varexp(envelopeId), varexp(senderId)));
		Cmd doAfter = ifNull(varexp(after), seq(), funcall(varexp(after), varexp(envelopeId), varexp(senderId)));
		if (hlc instanceof SyncDecl) {
			//a	          var Cresult := null;
			//b	          if(Cbefore != null) Cbefore();
			//c	          try {
			//d	            Cresult := +(s_normal_sync(Csender, Cnonce, Cargs, Cenvelope)); // note plussing.
			//e	          }
			//f	          catch {
			//g	            $(CsplitSync) => {}
			//h	          }
			//i	          finally {
			//j	            if(Cafter != null) Cafter();
			//k	          }
			//l	          if (Cresult ~ +CcallSucceeded) {
			//m	             syncReply(Cenvelope, CcallSucceeded);
			//n	          }
			//o	        }
			Id Cresult = DesugarUtils.gensym(start, "result");
			VarDecl a = vardecl(Cresult, lit(null));
			Cmd b = doBefore;
			FunCall s_normal_sync = funcall(varexp(hlc.gennedFunName), varexp(senderId), varexp(nonce), varexp(args),
					varexp(envelopeId));
			Cmd plussed_s_normal_sync = op(Op.POS, s_normal_sync);
			Cmd d = assign(Cresult, plussed_s_normal_sync);
			Case g = cas(patinterp(varexp(CsplitSync)), null, null, false, 0, seq());
			Cmd j = doAfter;
			Cmd cdefghijk = trycmd(d, Bard.list(g), j);
			Id CcallSucceeded = DesugarUtils.gensym(start, "callSucceeded");
			Cmd m = funcall(varexp(id("syncReply")), varexp(envelopeId), varexp(CcallSucceeded));
			Cmd resultMatch = tilde(varexp(Cresult), patplus(patvar(CcallSucceeded)));
			Cmd lmn = iff(resultMatch, m, seq());
			Cmd abcdefghijklmno = seq(a, b, cdefghijk, lmn);
			if (TestUtils.desugarServe) {
				System.err.println("commReaction sync = " + hlc);
				System.err.println("commReaction out == " + abcdefghijklmno);
			}
			return abcdefghijklmno;

		} else if (hlc instanceof AsyncDecl) {
			//		     => {
			//A		          if(Cbefore != null) Cbefore();
			//B		          try {
			//C		            (s_normal_sync(Csender, Cargs, Cenvelope)); 
			//D		          }
			//E		          finally {
			//F		            if(Cafter != null) Cafter();
			//G		          }
			//H		        }
			AsyncDecl async = (AsyncDecl) hlc;
			Cmd A = doBefore;
			Cmd C = funcall(varexp(hlc.gennedFunName), varexp(senderId), varexp(args), varexp(envelopeId));
			Cmd F = doAfter;
			Cmd BCDEFG = trycmd(C, Collections.EMPTY_LIST, F);
			Cmd ABCDEFGH = seq(A, BCDEFG);
			if (TestUtils.desugarServe) {
				System.err.println("commReaction async = " + hlc);
				System.err.println("commReaction out == " + ABCDEFGH);
			}
			return ABCDEFGH;
		} else {
			Doom.internalCatastrophe("What is this HighLevelCommunication?", hlc);
			return null;
		}
	}

	private Pat commRecord(HighLevelCommunication hlc, Id args, Id nonce) {
		if (hlc instanceof SyncDecl) {
			SyncDecl sync = (SyncDecl) hlc;
			PatRecordCtor patrec = patrec(id("sync"), patlit(hlc.name.str()), id("args"), patvar(args), id("nonce"),
					patvar(nonce));
			return patrec;
		} else if (hlc instanceof AsyncDecl) {
			AsyncDecl async = (AsyncDecl) hlc;
			return patrec(id("async"), patlit(hlc.name.str()), id("args"), patvar(args)
			// and no nonce
			);
		} else {
			Doom.internalCatastrophe("What is this HighLevelCommunication?", hlc);
			return null;
		}
	}

	private Cmd doServe(Serve serve) {
		// CserveC(beforefn, afterfn, timN, timCmdFn)
		ComponentInfo compInfo = serve.componentInfo;
		if (compInfo == null) {
			return null; // it's marked as an error somewhere else.
		}
		Id serveFunName = compInfo.serveId();

		AnonFun beforefn = makeFn(serve.before);
		AnonFun afterfn = makeFn(serve.after);
		Cmd timN = serve.timeout == null ? lit(null) : serve.timeout;
		AnonFun timCmdFn = fn(formals(), serve.timeoutCmd == null ? lit(null) : serve.timeoutCmd, false);

		final FunCall funcall = funcall(varexp(serveFunName), beforefn, afterfn, timN, timCmdFn);
		// try {funcall} catch {catchcases} <-- if catchcases given.
		// try {funcall} catch {x => println(x);} <-- if not given
		List<Case> cases;
		if (serve.cases.isEmpty()) {
			Id x = DesugarUtils.gensym(serve.start, "exn");
			PatVar patx = patvar(x); 
			Id println = id("println");
			FunCall printcmd = funcall(varexp(println), varexp(x));
			Case printit = cas(patx, null, null, false, 0, printcmd);
			cases = Bard.list(printit);
		}
		else {
			cases = serve.cases; // I think that it is safe not to copy them.
		}
		Cmd trycmd = trycmd(funcall, cases, null);
//		if (serve.cases.isEmpty()) {
//			System.err.println("DistDesugarer:!!: " + trycmd);
//		}
		return trycmd;
	}

	private AnonFun makeFn(ServeBlock sb) {
		if (sb == null) {
			return fn(formals(patwild(), patwild()), seq(), false);
		} else if (sb.formals == null) {
			return fn(formals(patwild(), patwild()), cmd(sb.cmd == null ? seq() : cmd(sb.cmd)), false);
		} else {
			Pat x = sb.formals.size() <= 0 ? patwild() : patvar(sb.formals.get(0));
			Pat y = sb.formals.size() <= 1 ? patwild() : patvar(sb.formals.get(1));
			assert (sb.formals.size() <= 2);
			return fn(formals(x, y), sb.cmd == null ? seq() : cmd(sb.cmd), false);
		}
	}

}
