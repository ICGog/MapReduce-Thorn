
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.converters;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.sun.java.swing.plaf.windows.WindowsTreeUI.ExpandedIcon;

import fisher.syn.*;

import fisher.syn.core.Id;
import fisher.syn.core.Op;
import fisher.syn.visitor.VanillaVisitCmd;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import static fisher.syn.core.Op.*;

public  class  CmdToPat extends VanillaVisitCmd<Object, Pat, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public FisherSource source;
	public String reasonForFailure = null;
	
	public Pat pat(Cmd syn) throws FisherException {
		if (syn==null) return null;
		return syn.accept(this, null);
	}

	public List<Pat> pats(List<? extends Cmd> syns) throws FisherException {
		List<Pat> L = new ArrayList<Pat>(syns.size());
		for (Cmd cmd : syns) {
			Pat p = pat(cmd); 
			if (p == null) return null;
			L.add(p);
		}
		return L;
	}
	
	@Override
	public Pat visit(Cmd syn, Object arg) throws FisherException {
		this.reasonForFailure = 
			"Expression is not a pattern: " + syn
			+ "\nOriginal source: " + source.original(syn)
			;
		return null;
	}

	@Override
	public Pat visit(VarExp syn, Object arg) throws FisherException {
		return new PatVar(syn.start, syn.end, syn.id);
	}
	
	@Override
	public Pat visit(Literal syn, Object arg) throws FisherException {
		return new PatLiteral(syn.start, syn.end, syn.value);
	}
	
	@Override
	public Pat visit(Parens syn, Object arg) throws FisherException {
		return syn.exp.accept(this, arg);
	}
	
	@Override
	public Pat visit(ListCtor syn, Object arg) throws FisherException {
		List<PatListBit> bits = new ArrayList<PatListBit>(syn.bits.size());
		for (ListBit listBit : syn.bits) {
			PatListBit plb;
			if (listBit instanceof ListBitEllip) {
				ListBitEllip ellip = (ListBitEllip) listBit;
				Pat pat = pat(ellip.exp);
				if (pat == null) return null; // Whole thing isn't a pattern.
				plb = new PatListBitEllip(listBit.start, listBit.end, pat);
			}
			else if (listBit instanceof ListBitExp) {
				ListBitExp exp = (ListBitExp) listBit;
				Pat pat = pat(exp.exp);
				if (pat == null) return null; // Whole thing isn't a pattern.
				plb = new PatListBitExp(exp.start, exp.end, pat);
			}
			else {
				Doom.internal("I don't know how to convert this list constructor bit into a pattern", listBit, listBit.getClass());
				return null;
			}
			
			bits.add(plb);
		}
		return new PatListCtor(syn.start, syn.end, bits);
	}
	
	
	@Override
	public Pat visit(WildcardExp syn, Object arg) throws FisherException {
		return new PatWildcard(syn.start, syn.end);
	}
	
	@Override
	public Pat visit(InterpolationExp syn, Object arg) throws FisherException {
		return new PatInterpolation(syn.start, syn.end, syn.exp);
	}
	
	public Pat visit(EvalTestExpExp syn, Object arg) throws FisherException {
		return new PatEvalTestExp(syn.start, syn.end, syn.exp);		
	};
	
	public Pat visit(OpExp syn, Object arg) throws FisherException {
		Op op = syn.op;
		switch (op) {
		case AND: return doAnd(syn);
		case OR: return doOr(syn);
		case NOT: return doNot(syn);
		case POS: return doPos(syn);
		case DOTDOT : return doRange(syn);
		case FDIV : return doSlash(syn);
		default:
			this.reasonForFailure = 
				"The operation " + op + " is not meaningful in a pattern: " + syn
				+ "\nOriginal source: " + source.original(syn)
				;
			return null;
		}
	};
	
	public PatSlash doSlash(OpExp syn) throws FisherException {
		if (syn.operands.size() != 2) return null;
		Cmd re = syn.operands.get(0);
		Cmd patexp = syn.operands.get(1);
		Pat pat = pat(patexp);
		// Original = re / pat
		// Desugaring = it.`match/`(re) ~ +pat
		ItExp it = new ItExp(syn.start, syn.end);
		MethodCall match_call = new MethodCall(syn.start, syn.end, it, new Id(syn.start, "match/"), Bard.list(re));
		Pat plusPat = new PatNotNull(syn.start, syn.end, pat);
		Pat desugared = 
			new PatMatchSomethingElse(syn.start, syn.end, match_call, plusPat);
		PatSlash slash = new PatSlash(syn.start, syn.end, re, pat, desugared);
		return slash;
	}
	
	public PatRange doRange(OpExp syn) throws FisherException {
		List<Pat> pats = pats(syn.operands);
		if (pats.size() != 2) return null;
		return new PatRange(syn.start, syn.end, pats.get(0), pats.get(1));
	}
	
	public PatAnd doAnd(OpExp syn) throws FisherException {
		List<Pat> pats = pats(syn.operands);
		if (pats == null) return null;
		return new PatAnd(syn.start, syn.end, pats);
	}
	
	public PatOr doOr(OpExp syn) throws FisherException {
		List<Pat> pats = pats(syn.operands);
		if (pats == null) return null;
		return new PatOr(syn.start, syn.end, pats);
	}
	
	public PatNot doNot(OpExp syn) throws FisherException {
		if (syn.operands.size() != 1) return null; 
		Pat sub = pat(syn.operands.get(0));
		if (sub == null) return null;
		return new PatNot(syn.start, syn.end, sub); 
	}
	
	public PatNotNull doPos(OpExp syn) throws FisherException {
		if (syn.operands.size() != 1) {
			this.reasonForFailure = 
				"The binary operation + is not meaningful in a pattern: " + syn
				+ "\nOriginal source: " + source.original(syn)
				;
			return null; 
		}
		Pat sub = pat(syn.operands.get(0));
		if (sub == null) return null;
		return new PatNotNull(syn.start, syn.end, sub); 
	}
	
	@Override
	public Pat visit(MatchExp syn, Object arg) throws FisherException {
		return new PatMatchSomethingElse(syn.start, syn.end, syn.subject, syn.pat);
	}
	
	public static QualName repeatedFieldRef2QualName(FieldRef fr) throws FisherException {
		List<Id> ids = Bard.list(fr.field);
		Cmd cursor = fr.target;
		while (cursor != null) {
			if (cursor instanceof VarExp) {
				VarExp ve = (VarExp) cursor;
				ids.add(0, ve.id);
				break;
			}
			else if (cursor instanceof FieldRef) {
				FieldRef fr2 = (FieldRef) cursor;
				cursor = fr2.target;
				ids.add(0,fr2.field);
			}
			else {
				fr.flag(DangerLevel.ERROR, "This context demands a qualname: id.id.id", "Any number of id's really, but not " + cursor);
			}
		}
		return new QualName(fr.start, fr.end, ids);
	}
	
	@Override
	public Pat visit(FunCall syn, Object arg) throws FisherException {
		if (syn.function instanceof VarExp) {
			VarExp ve = (VarExp) syn.function;
			Id function = ve.id; 
			List<Pat> subpats = pats(syn.args);
			if (subpats == null) return null;
			QualName qn = new QualName(function.start, function.end, Bard.list(function));
			return new PatExtract(syn.start, syn.end, qn, subpats);
		}
		else if (syn.function instanceof FieldRef) {
			FieldRef fr = (FieldRef) syn.function;
			QualName qn = repeatedFieldRef2QualName(fr);
			List<Pat> subpats = pats(syn.args);
			return new PatExtract(syn.start, syn.end, qn, subpats);
		}
		else {
			return null;
		}
	}
	
	@Override
	public Pat visit(MethodCall syn, Object arg) throws FisherException {
		if (syn.target instanceof VarExp) { // modu.clas(x), but looks like a method call.
			VarExp ve = (VarExp) syn.target;
			QualName qn = new QualName(syn.target.start, syn.target.end, Bard.list(ve.id, syn.method));
			return new PatExtract(syn.start, syn.end, qn, pats(syn.args));
		}
		else {
			return null;
		}
	}
	
	@Override
	public Pat visit(ExpExtract syn, Object arg) throws FisherException {
		return new PatExtract(syn.start, syn.end, syn.patname, pats(syn.subpats));
	}
	
	
	@Override
	public Pat visit(TypedExp syn, Object arg) throws FisherException {
		Pat pat = pat(syn.exp);
		return new PatTypeTest(syn.start, syn.end, pat, syn.type);
	}
	
	@Override
	public Pat visit(RecordCtor syn, Object arg) throws FisherException {
		List<PatRecordField> fields = new ArrayList<PatRecordField>(syn.fields.size());
		for (RecordField fld : syn.fields) {
			Pat p = pat(fld.exp);
			if (p == null) {
				return null;
			}
			PatRecordField pfld = new PatRecordField(fld.start, fld.end, fld.id, p);
			fields.add(pfld);
		}
		return new PatRecordCtor(syn.start, syn.end, fields);
	}
	
	@Override
	public Pat visit(DotMethodCallExp syn, Object arg) throws FisherException {
		return new PatMethodCall(syn.start, syn.end, syn.methodName, 
				pat(syn.subpat)
		);
	}
	
}
