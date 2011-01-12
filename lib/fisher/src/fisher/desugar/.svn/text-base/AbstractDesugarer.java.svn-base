
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

import static fisher.util.Bard.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import fisher.eval.EvalUtil;
import fisher.ingest.Ingester;
import fisher.parser.Token;
import fisher.syn.AnonFun;
import fisher.syn.Assign;
import fisher.syn.AssignTarget;
import fisher.syn.AssignToId;
import fisher.syn.Bind;
import fisher.syn.Case;
import fisher.syn.Cmd;
import fisher.syn.Comparison;
import fisher.syn.ComparisonBit;
import fisher.syn.Formals;
import fisher.syn.FunBody;
import fisher.syn.FunCall;
import fisher.syn.FunDecl;
import fisher.syn.If;
import fisher.syn.ListBit;
import fisher.syn.ListBitExp;
import fisher.syn.ListCtor;
import fisher.syn.Literal;
import fisher.syn.MatchExp;
import fisher.syn.MethodCall;
import fisher.syn.MonoBody;
import fisher.syn.OpExp;
import fisher.syn.Pat;
import fisher.syn.PatAnd;
import fisher.syn.PatInterpolation;
import fisher.syn.PatListBitExp;
import fisher.syn.PatListCtor;
import fisher.syn.PatLiteral;
import fisher.syn.PatNotNull;
import fisher.syn.PatRecordCtor;
import fisher.syn.PatRecordField;
import fisher.syn.PatVar;
import fisher.syn.PatWildcard;
import fisher.syn.Probe;
import fisher.syn.QualName;
import fisher.syn.RecordCtor;
import fisher.syn.RecordField;
import fisher.syn.Recv;
import fisher.syn.Send;
import fisher.syn.Seq;
import fisher.syn.TableKey;
import fisher.syn.Try;
import fisher.syn.TypeConstraint;
import fisher.syn.TypeConstraints;
import fisher.syn.VarDecl;
import fisher.syn.VarExp;
import fisher.syn.core.ComparisonOp;
import fisher.syn.core.Id;
import fisher.syn.core.Op;
import fisher.syn.core.Syntax;
import fisher.syn.visitor.VanillaWalker;
import fisher.test.Prober;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public abstract  class  AbstractDesugarer    { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static List<Cmd> deepCopyCmds(Token start, Token end, List<? extends Cmd> cmds) {
		List<Cmd> L = new ArrayList<Cmd>(cmds.size());
		for (Cmd cmd : cmds) {
			Cmd cp = (Cmd) cmd.ingestedDeepCopy(start, end);
			L.add(cp);
		}
		return L;
	}

	protected Token start, end;

	public AbstractDesugarer(Token start, Token end) {
		super();
		this.start = start;
		this.end = end;
	}

	protected Id id(Id id) {
		return id == null ? null : (Id) id.ingestedDeepCopy(start, end);
	}
	
	protected Id id(String s) {
		return new Id(start, s);
	}
	
	protected ListCtor listctor(List<Cmd> elements) {
		List<ListBit> bits = Bard.list();
		for (Cmd el : elements) {
			bits.add(new ListBitExp(start, end, cmd(el)));
		}
		return new ListCtor(start, end, bits);
	}
	
	protected Cmd cmd(Cmd cmd) {
		return cmd == null ? null : (Cmd) cmd.ingestedDeepCopy(start, end);
	}
	
	protected Send send(Cmd rcvr, Cmd msg, Cmd tokens) {
		return new Send(start, end, cmd(rcvr), cmd(msg), cmd(tokens));
	}
	
	protected QualName qn(QualName q) {
		List<Id> ids = new ArrayList<Id>(q.ids.size());
		for (Id id : q.ids) {
			ids.add(id(id));
		}
		QualName qn = new QualName(start, end, ids);
		return qn;
	}
	
	protected Seq seq(Cmd... cmds) {
		return new Seq(start, end, Bard.array2list(cmds));
	}

	protected If iff(Cmd test, Cmd Then, Cmd Else) {
		return new If(start, end, (Cmd) test.ingestedDeepCopy(start, end), Then == null ? null : (Cmd) Then
				.ingestedDeepCopy(start, end), Else == null ? null : (Cmd) Else.ingestedDeepCopy(start, end), false, false);
	}

	protected List<Cmd> deepCopyCmds(List<? extends Cmd> cmds) {
		return deepCopyCmds(start, end, cmds);
	}
	
	protected List<Cmd> deepCopyCmds(Cmd[] cmds) {
		return deepCopyCmds(Bard.array2list(cmds));
	}
	
	protected List<Case> deepCopyCases(List<Case> cases) {
		List<Case> cp = Bard.list();
		for (Case c : cases) {
			cp.add(  (Case) c.ingestedDeepCopy(start, end));
		}
		return cp;
	}

	protected Cmd op(Op o, Cmd... cmds) {
		return new OpExp(start, end, o, deepCopyCmds(Bard.array2list(cmds)));
	}
	
	protected RecordField rf(Id fieldname, Cmd cmd) {
		return new RecordField(start, end, id(fieldname), cmd == null ? null : (Cmd) cmd.ingestedDeepCopy(start, end) );
	}

	public RecordCtor rec(Object ... fieldsAndValues) {
		if (fieldsAndValues.length % 2 == 1) {
			Doom.internalCatastrophe("More fields than value!  or maybe v-v!", (Syntax)fieldsAndValues[0]);
		}
		List<RecordField> rfs = Bard.list();
		for(int i = 0; i < fieldsAndValues.length; i+= 2) {
			RecordField recfld = rf((Id)  fieldsAndValues[i], (Cmd) fieldsAndValues[i+1]);
			rfs.add(recfld);
		}
		return new RecordCtor(start, end, rfs);		
	}
	
	public PatRecordCtor patrec(Object ... fieldsAndValues) {
		if (fieldsAndValues.length % 2 == 1) {
			Doom.internalCatastrophe("More fields than value!  or maybe v-v!", (Syntax)fieldsAndValues[0]);
		}
		List<PatRecordField> rfs = Bard.list();
		for(int i = 0; i < fieldsAndValues.length; i+= 2) {
			PatRecordField recfld =
				new PatRecordField(start, end, (Id)  fieldsAndValues[i], (Pat)fieldsAndValues[i+1]);
			rfs.add(recfld);
		}
		return new PatRecordCtor(start, end, rfs);
	}
	
	public Recv recv(Cmd timN, Cmd timCmd, Case... caseo) {
		List<Case> cases = Bard.array2list(caseo);
		Recv r = new Recv(start, end, cases,timN, timCmd);
		try {
			Ingester.check(r);
		} catch (FisherException e) {
			e.printStackTrace();
		}
		return r;		
	}
	
	protected PatVar patvar(Id id) {
		return new PatVar(start, end, id(id));
	}
	
	protected PatWildcard patwild() {
		return new PatWildcard(start,end);
	}
	
	protected PatNotNull patplus(Pat subpat) {
		return new PatNotNull(start, end, pat(subpat));
	}
	
	
	protected PatLiteral patlit(Object val) {
		try {
			PatLiteral pl  = new PatLiteral(start, end, val);
			Ingester.check(pl); // get the thing field set.
			if (pl.thing == null && val != null) {
				assert(pl.thing != null);
			}
			return pl;
		} catch (FisherException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	protected Pat patlist(List<Pat> patel) {
		List<fisher.syn.PatListBit> bits = Bard.list();
		for(Pat pe : patel) {
			bits.add(new PatListBitExp(start, end, pat(pe)));
		}
		return new PatListCtor(start, end, bits);
	}
	
	protected Pat patlist(Pat... patel) {
		List<fisher.syn.PatListBit> bits = Bard.list();
		for(Pat pe : patel) {
			bits.add(new PatListBitExp(start, end, pat(pe)));
		}
		return new PatListCtor(start, end, bits);
	}
	
	protected PatInterpolation patinterp(Cmd cmd) {
		return new PatInterpolation(start, end, cmd(cmd));
	}

	protected VarExp varexp(Id id) {
		return new VarExp(start, end, (Id) id.ingestedDeepCopy(start, end));
	}

	protected AssignTarget varass(Id id) {
		return new AssignToId(start, end, (Id) id.ingestedDeepCopy(start, end));
	}
	
	protected MethodCall meth(Cmd recvr, String methodName, Cmd... args) {
		return new MethodCall(start, end, recvr, new Id(start, methodName), deepCopyCmds(args));
	}
	
	protected MatchExp tilde(Cmd subject, Pat pat) {
		return new MatchExp(start, end, cmd(subject), pat(pat));
	}
	
	protected Try trycmd(Cmd stuff, List<Case> catches, Cmd dofinally) {
		return new Try(start, end, cmd(stuff), deepCopyCases(catches), cmd(dofinally) );
	}
	
	public Case cas(Pat pat, Pat from, Pat envelope, boolean hasPrio, int prio, Cmd body) {
		return cas2(pat, from, envelope, hasPrio, prio, false, body);
	}
	public Case cas2(Pat pat, Pat from, Pat envelope, boolean hasPrio, int prio, boolean checked, Cmd body) {
		return new Case(start, end, pat, from, envelope, hasPrio, prio, checked, body);
	}
	
	public Pat patand(Pat ... pats) {
		List<Pat> subpats = Bard.list();
		for (Pat pat : pats) {
			subpats.add(pat(pat));
		}
		return new PatAnd(start, end, subpats);
	}
	
	protected FunCall funcall(Cmd fun, Cmd... args) {
		List<Cmd> args2 = Bard.list();
		for (Cmd cmd : args) {
			args2.add(cmd(cmd));
		}
		return new FunCall(start, end, cmd(fun), args2);
	}
	
	
	protected AnonFun fn(Formals formals, Cmd body, boolean isPure) {
		return new AnonFun(start, end, funbody(monobody(null, formals, null, null, false, 0, body, isPure)), isPure);
	}
	
	protected FunDecl fundecl(Id name, MonoBody monobody) {
		return new FunDecl(start, end, name, funbody(monobody), monobody.isMarkedPure());
	}
	
	protected FunBody funbody(MonoBody monobody) {
		return new FunBody(start, end, Bard.list(monobody), monobody.isMarkedPure());
	}

	protected MonoBody monobody(Id id, Formals formals, Pat from, Pat envel, boolean hasPrio, int prio, Cmd body, boolean isPure) {
		return monobody2(id, formals, from, envel, hasPrio, prio, body, false, isPure);
	}
	protected MonoBody monobody2(Id id, Formals formals, Pat from, Pat envel, boolean hasPrio, int prio, Cmd body, boolean checked, boolean isPure) {
		return new MonoBody(start, end, id(id), formals, pat(from), pat(envel),  hasPrio, prio, cmd(body), checked, isPure);
	}
	
	protected Formals formals(Pat... formlls) {
		List<Pat> formals = Bard.list();
		for (Pat pat : formlls) {
			formals.add(pat(pat));
		}
		return new Formals(start, end, formals);
	}
	
	protected Cmd eq(Cmd a, Cmd b) {
		return new Comparison(start, end, cmd(a), Bard.list(new ComparisonBit(start, end, ComparisonOp.EQ, cmd(b))));
	}
	
	// if (v == null) ifNull else notNull
	protected Cmd ifNull(Cmd v, Cmd ifNull, Cmd notNull) {
		return iff(eq(cmd(v), lit(null)), cmd(ifNull), cmd(notNull));
	}
	
	protected Pat pat(Pat pat) {
		return pat == null ? null : (Pat) pat.ingestedDeepCopy(start, end);
	}
	
	protected Assign assign(Id id, Cmd exp) {
		Assign assign = new Assign(start, end, list(varass(id)), list((Cmd)exp.ingestedDeepCopy(start, end)));
		return assign;
	}

	
	
	protected TypeConstraints tycon(Id id, String ... ss) {
		List<TypeConstraint> cons = list();
		for (String s : ss) {
			cons .add(new TypeConstraint(id.start, id.end, id(s)));
		}
		return new TypeConstraints(id.start, id.end, cons);
	}
	protected TypeConstraints tycon(String ... ss) {
		List<TypeConstraint> cons = list();
		for (String s : ss) {
			cons .add(new TypeConstraint(start, end, id(s)));
		}
		return new TypeConstraints(start, end, cons);
	}
	
	protected TypeConstraints tyclone(TypeConstraints tc) {
		return ((TypeConstraints) tc.ingestedDeepCopy(tc.start, tc.end));
	}
	
	protected VarDecl vardecl(Id id, Cmd init) {
		return vardecl(id, init, tycon(id));
	}
	protected VarDecl vardecl(Id id, Cmd init, TypeConstraints tycon) {
		VarDecl vd = new VarDecl(start, end, id(id),
				init == null ? null : cmd(init), tycon
				);
		return vd;
	}

	protected PatVar varpat(Id id) {
		return new PatVar(start, end, id(id));
	}

	protected Bind bind(Id id, Cmd exp) {
		Bind bind = new Bind(start, end, varpat(id), cmd(exp));
		return bind;
	}
	

	protected Assign assign(Id id1, Id id2, Cmd exp1, Cmd exp2) {
		Assign assign = new Assign(start, end, list(varass(id1), varass(id2)), list(exp1, exp2));
		return assign;
	}

	protected Probe probe(Prober prober, List<Cmd> exps) { 
		Probe probe = new Probe(start, end, new Id(start, prober.text), exps, -1);
		probe.prober = prober;
		return probe;
	}

	protected Literal lit(Object val) {
		return new Literal(start, start, val);
	}
	
	protected TableKey tablekey(TableKey tk) {
		return (TableKey) (tk.ingestedDeepCopy(start, end)); 
	}
	

}
