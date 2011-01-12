
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.queries;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import fisher.syn.Pat;
import fisher.syn.PatAnd;
import fisher.syn.PatEvalTestExp;
import fisher.syn.PatExtract;
import fisher.syn.PatInterpolation;
import fisher.syn.PatListBit;
import fisher.syn.PatListBitEllip;
import fisher.syn.PatListBitExp;
import fisher.syn.PatListCtor;
import fisher.syn.PatLiteral;
import fisher.syn.PatMatchSomethingElse;
import fisher.syn.PatNot;
import fisher.syn.PatNotNull;
import fisher.syn.PatOr;
import fisher.syn.PatRecordCtor;
import fisher.syn.PatRecordField;
import fisher.syn.PatTypeTest;
import fisher.syn.PatVar;
import fisher.syn.PatWildcard;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.visitor.VanillaWalkPat;
import fisher.syn.visitor.VanillaWalker;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  PatVars_Defined_By_Pattern extends VanillaWalkPat<List<PatVar>, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private static PatVars_Defined_By_Pattern IT = new PatVars_Defined_By_Pattern();
	
	
	public static List<PatVar> of(Pat syn) throws FisherException {
		List<PatVar> S = new ArrayList<PatVar>(1);
		syn.accept(IT, S);
		return S;
	}
	
	@Override
	public void visit(Pat syn, List<PatVar> arg) throws FisherException {
		Doom.internal("Trying to find the ids defined by this syntax, but I don't know how", syn);
	}
	
	@Override
	public void visit(PatVar syn, List<PatVar> arg) throws FisherException {
		for (PatVar patVar : arg) {
			if(patVar.sameNameAs(syn)) {
				syn.flag(DangerLevel.ERROR, "Two bindings for the same variable in one pattern: " + syn, "", syn, patVar);
			}
		}
		arg.add(syn);
	}
	
	@Override
	public void visit(PatListCtor syn, List<PatVar> arg) throws FisherException {
		List<PatListBit> bits = syn.bits;
		for (PatListBit bit : bits) {
			// bit.accept(this, arg);
			if (bit instanceof PatListBitExp) {
				PatListBitExp bitexp = (PatListBitExp) bit;
				bitexp.pat.accept(this, arg);
			}
			else if (bit instanceof PatListBitEllip) {
				PatListBitEllip ellip = (PatListBitEllip) bit;
				ellip.pat.accept(this,arg);
			}
			else {
				Doom.internalCatastrophe("Missed case!", syn);
			}
		}
	}
	


	@Override
	public void visit(PatAnd syn, List<PatVar> arg) throws FisherException {
		for (Pat conjunct : syn.subpats) {
			conjunct.accept(this, arg);
		}
	}

	@Override
	public void visit(PatExtract syn, List<PatVar> arg)
			throws FisherException {
		for (Pat subpat : syn.subpats) {
			subpat.accept(this, arg);
		}
	}
	
	@Override
	public void visit(PatTypeTest syn, List<PatVar> arg) throws FisherException {
		syn.subpat.accept(this, arg);
	}
	
	@Override
	public void visit(PatEvalTestExp syn, List<PatVar> arg)
			throws FisherException {
		// Produces no variables.
	}
	
	@Override
	public void visit(PatInterpolation syn, List<PatVar> arg)
			throws FisherException {
		// produces no variables
	}
	
	@Override
	public void visit(PatLiteral syn, List<PatVar> arg) throws FisherException {
		// produces no variables
	}
	
	@Override
	public void visit(PatMatchSomethingElse syn, List<PatVar> arg)
			throws FisherException {
		// 
		syn.pat.accept(this, arg);
	}
	
	@Override
	public void visit(PatNot syn, List<PatVar> arg) throws FisherException {
		// produces no variables -- see the spec!
	}
	
	@Override
	public void visit(PatNotNull syn, List<PatVar> arg) throws FisherException {
		syn.subpat.accept(this, arg);
	}
	
	@Override
	public void visit(PatRecordCtor syn, List<PatVar> arg)
			throws FisherException {
		for (PatRecordField field : syn.fields) {
			field.pat.accept(this, arg);
		}
	}
	
	@Override
	public void visit(PatWildcard syn, List<PatVar> arg) throws FisherException {
		// produces no variables -- see the spec!
	}
	
	@Override
	public void visit(PatOr syn, List<PatVar> arg) throws FisherException {
		List<PatVar> common = new ArrayList<PatVar>();
		List<PatVar> current = new ArrayList<PatVar>();
		Set<String> currentNames = new HashSet<String>();
		Set<String> commonNames = new HashSet<String>();
		syn.subpats.get(0).accept(this, common);
		// common = the ones that they all should produce.
		
		// Grab the common names: 
		for (PatVar pv : common) {
			commonNames.add(pv.id.str());
		}
		
		for(int i = 1; i < syn.subpats.size(); i ++) {
			current.clear();
			currentNames.clear();
			Pat pat = syn.subpats.get(i);
			pat.accept(this, current);
			for (PatVar pv : current) {
				currentNames.add(pv.id.str());
			}
			if(commonNames.equals(currentNames)) {
				// all is well
			}
			else {
				pat.flag(DangerLevel.ERROR, "Every disjunct in a pattern must define the same variables.",
						"The first disjunct defines: " + Bard.sep(common, ", "),
						"Disjunct number " + i + " defines " + Bard.sep(current, ", ")
						);
			}
		}
		arg.addAll(common);
	}
	
}
