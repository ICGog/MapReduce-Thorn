
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.eval;

import java.util.List;

import fisher.desugar.ComponentDeclDesugarer;
import fisher.eval.interfaces.Fieldiferous;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.Applyable;
import fisher.runtime.BoolTh;
import fisher.runtime.ClassDynamicTh;
import fisher.runtime.ClosureTh;
import fisher.runtime.ComponentTh;
import fisher.runtime.ExtensionClass;
import fisher.runtime.JavalyFunImpl;
import fisher.runtime.ListTh;
import fisher.runtime.ModuleDynamicTh;
import fisher.runtime.ObjectTh;
import fisher.runtime.OrdTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.TableTh;
import fisher.runtime.Thing;
import fisher.runtime.VarCell;
import fisher.runtime.auxil.FisherReturn;
import fisher.runtime.auxil.Row;
import fisher.runtime.auxil.ThornThrow;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.DistUtil;
import fisher.runtime.dist.SiteData;
import fisher.statics.ClassStatic;
import fisher.statics.MethodSig;
import fisher.statics.Seal;
import fisher.statics.SealForVal;
import fisher.syn.AbstractStringBit;
import fisher.syn.AnonFun;
import fisher.syn.AnonObj;
import fisher.syn.Assign;
import fisher.syn.AssignTarget;
import fisher.syn.AssignToFieldOfSubscripted;
import fisher.syn.AssignToId;
import fisher.syn.AssignToMap;
import fisher.syn.AssignToSubscripted;
import fisher.syn.AssignTofield;
import fisher.syn.AsyncStmt;
import fisher.syn.Bind;
import fisher.syn.BracketCall;
import fisher.syn.Break;
import fisher.syn.Case;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.Cmd;
import fisher.syn.CmdsInAList;
import fisher.syn.Comparison;
import fisher.syn.ComparisonBit;
import fisher.syn.ComponentDecl;
import fisher.syn.Continue;
import fisher.syn.FieldRef;
import fisher.syn.For;
import fisher.syn.FunCall;
import fisher.syn.FunDecl;
import fisher.syn.If;
import fisher.syn.ImportStmt;
import fisher.syn.ItExp;
import fisher.syn.JavalyClassDecl;
import fisher.syn.JavalyFun;
import fisher.syn.ListBit;
import fisher.syn.ListBitEllip;
import fisher.syn.ListBitExp;
import fisher.syn.ListCtor;
import fisher.syn.Literal;
import fisher.syn.MapCtor;
import fisher.syn.Match;
import fisher.syn.MatchExp;
import fisher.syn.MethDecl;
import fisher.syn.MethodCall;
import fisher.syn.OpABExp;
import fisher.syn.OpExp;
import fisher.syn.Ord;
import fisher.syn.Parens;
import fisher.syn.PatVar;
import fisher.syn.Probe;
import fisher.syn.QueryAfter;
import fisher.syn.QueryFirst;
import fisher.syn.QueryGroup;
import fisher.syn.QueryListComprehension;
import fisher.syn.QueryQuantifierCount;
import fisher.syn.QueryQuantifierEvery;
import fisher.syn.QueryQuantifierSome;
import fisher.syn.QuerySort;
import fisher.syn.QueryTable;
import fisher.syn.RecordCtor;
import fisher.syn.Recv;
import fisher.syn.Return;
import fisher.syn.Send;
import fisher.syn.Seq;
import fisher.syn.Serve;
import fisher.syn.Signature;
import fisher.syn.Spawn;
import fisher.syn.SpawnByComponentName;
import fisher.syn.StringBitText;
import fisher.syn.StringBitVar;
import fisher.syn.StringWithInterpolations;
import fisher.syn.SuperCall;
import fisher.syn.SuperCtorCall;
import fisher.syn.SyncStmt;
import fisher.syn.Table;
import fisher.syn.This;
import fisher.syn.Throw;
import fisher.syn.Try;
import fisher.syn.TypedExp;
import fisher.syn.Valof;
import fisher.syn.VarDecl;
import fisher.syn.VarExp;
import fisher.syn.While;
import fisher.syn.core.ComparisonOp;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.Classlike;
import fisher.syn.visitor.VanillaVisitCmd;
import fisher.syn.visitor.VisitCmd;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public  class  Computer extends VanillaVisitCmd<Framelike, Thing, FisherException> implements
		VisitCmd<Framelike, Thing, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private final Evaller evaller;

	/* package */Computer(Evaller evaller) {
		super();
		this.evaller = evaller;
	}

	public Thing visitEach(List<Cmd> cmds, Framelike frame) throws FisherException {
		Thing res = Thing.NULL;
		for (Cmd cmd : cmds) {
			res = evaller.eval(cmd, frame);
		}
		return res;
	}

	@Override
	public Thing visit(Cmd syn, Framelike frame) throws FisherException {
		Doom.internal("Does not compute!  Evaluation of " + syn.genericName() + " is not yet implemented.", syn, frame);
		return null;
	}

	@Override
	public Thing visit(Literal syn, Framelike arg) throws FisherException {
		return EvalUtil.thingify(syn.value, syn);
	}

	@Override
	public Thing visit(Probe syn, Framelike frame) throws FisherException {
		TestUtils.hit(syn);
		Thing res = syn.prober.compute(syn, frame, evaller);
		return res;
	}

	@Override
	public Thing visit(VarDecl syn, Framelike frame) throws FisherException {
		doVarDecl(syn, syn.init, syn.var, frame, evaller);
		return EvalUtil.nip(syn);
	}

	public static void doVarDecl(Syntax src, Cmd initCmd, Id var, Framelike framelike, Evaller evaller)
			throws FisherException {
		Thing init = initCmd == null ? Thing.NULL : evaller.eval(initCmd, framelike);
		List<Thing> types = var.seal().types(framelike, src);
		VarCell cell = new VarCell(init, var.seal(), types);
		framelike.store(var.seal(), cell, src);
	}

	@Override
	public Thing visit(Assign syn, Framelike frame) throws FisherException {
		if (syn.lhs.size() == 1 && syn.rhs.size() == 1) {
			Thing value = evaller.eval(syn.rhs.get(0), frame);
			AssignTarget asst = syn.lhs.get(0);
			return doOneAssignment(syn, frame, value, asst);
		} else {
			// Evaluate the RHSses in one block, then do all the assignments.
			Thing[] rhsses = evaller.evalAll(syn.rhs, frame);
			int i = 0;
			for (Thing thing : rhsses) {
				AssignTarget asst = syn.lhs.get(i);
				doOneAssignment(syn, frame, thing, asst);
				i++;
			}
			return EvalUtil.nip(syn);
		}
	}

	@Override
	public Thing visit(OpABExp syn, Framelike frame) throws FisherException {
		Thing change = evaller.eval(syn.amount, frame);
		AssignTarget target = syn.target;
		if (target instanceof AssignToId) {
			AssignToId ati = (AssignToId) target;
			Seal seal = ati.id.seal();
			VarCell cell = frame.LValue(seal, syn);
			Thing oldValue = cell.Rvalue();
			Thing value = syn.opab.op.simpleEval(new Thing[] { oldValue, change }, syn, frame);
			//			seal.check(frame, value, syn);
			cell.setCargo(value, syn);
			return value;
		} else if (target instanceof AssignTofield) {
			AssignTofield atf = (AssignTofield) target;
			// atf is of the form A.f
			Thing A = evaller.eval(atf.target, frame);
			Seal f = atf.field.seal();
			return opAbToField(syn, frame, change, A, f);

		} else if (target instanceof AssignToFieldOfSubscripted) {
			// atfos is of the form A(b).f
			AssignToFieldOfSubscripted afs = (AssignToFieldOfSubscripted) target;
			Cmd array = afs.array;
			List<Cmd> subscripts2 = afs.subscripts;
			Thing[] subscripts = evaller.evalAll(subscripts2, frame);
			Id field = afs.field;
			Thing A = evaller.eval(array, frame);
			if (A instanceof TableTh) {
				TableTh table = (TableTh) A;
				final RecordTh oldRow = table.getRow(subscripts, target, 0).asRecordTh();
				Thing oldValue = oldRow.getField(field.str(), target);
				Thing value = syn.opab.op.simpleEval(new Thing[] { oldValue, change }, syn, frame);
				table.mutateField(subscripts, field.str(), value, syn, 0, false);
				return value;
			} else if (A instanceof OrdTh) {
				OrdTh ord = (OrdTh) A;
				RecordTh oldRow = ord.get(subscripts, syn, false);
				String fieldName = field.str();
				Thing oldValue = oldRow.getField(fieldName, syn);
				Thing value = syn.opab.op.simpleEval(new Thing[] { oldValue, change }, syn, frame);
				ord.mutateField(subscripts, fieldName, value, syn, 0, false);
				return value;
			} else {
				// A is not a table.  Apply it to the arguments, as per function application.
				Thing Asub = doFunCall(syn, frame, A, subscripts);
				return opAbToField(syn, frame, change, Asub, field.seal());
				//return doAssignToField(syn, value, Asub, field.seal());
			}
		} else if (target instanceof AssignToMap) {
			AssignToMap atom = (AssignToMap) target;
			String operation = "[]:=";
			Cmd array = atom.map;
			Thing A = evaller.eval(array, frame);
			List<Cmd> subscripts2 = atom.subscripts;
			Thing[] subscripts = evaller.evalAll(subscripts2, frame);
			if (A instanceof TableTh) {
				TableTh table = (TableTh) A;
				final RecordTh oldRow = table.getRow(subscripts, target, 0).asRecordTh();
				String mapFieldName = table.mapColInfo.name;
				Thing oldValue = oldRow.getField(mapFieldName, target);
				Thing value = syn.opab.op.simpleEval(new Thing[] { oldValue, change }, syn, frame);
				table.mutateField(subscripts, mapFieldName, value, syn, 0, false);
				return value;
			}
			if (A instanceof OrdTh) {
				OrdTh ord = (OrdTh) A;
				RecordTh oldRow = ord.get(subscripts, syn, false);
				Thing oldValue = oldRow.getField(ord.mapColInfo.name, syn);
				Thing value = syn.opab.op.simpleEval(new Thing[] { oldValue, change }, syn, frame);
				ord.Cmapput(syn.opab.str, new Thing[] { subscripts[0], value }, evaller, frame, syn);
				return value;
			} else {
				Doom.notYet();
				return null;
			}

		} else {
			// We ought to do the other kinds of assignment, like a.b := c
			// and a(b) := c.
			Doom.notYet();
			return null;
		}
	}

	private Thing opAbToField(OpABExp syn, Framelike frame, Thing change, Thing A, Seal f) throws FisherException,
			FisherRuntimeException {
		if (A instanceof ObjectTh) {
			ObjectTh FA = (ObjectTh) A;
			Thing oldValue = FA.RValue(f, syn);
			Thing value = syn.opab.op.simpleEval(new Thing[] { oldValue, change }, syn, frame);
			FA.storeIntoVarField(f, value, syn);
			return value;
		} else {
			Doom.runtime("Can't assign to fields of a " + A.typeString(), syn, A, f);
			return null;
		}
	}

	private Thing doOneAssignment(Assign syn, Framelike frame, Thing value, AssignTarget asst) throws FisherException,
			FisherRuntimeException {
		if (asst instanceof AssignToId) {
			AssignToId ati = (AssignToId) asst;
			Seal seal = ati.id.seal();
			VarCell cell = frame.LValue(seal, syn);
			//			seal.check(frame, value, syn);
			cell.setCargo(value, syn);
			return EvalUtil.nip(syn);
		} else if (asst instanceof AssignTofield) {
			AssignTofield atf = (AssignTofield) asst;
			// atf is of the form A.f
			Cmd target = atf.target;
			Id field = atf.field;
			Thing A = evaller.eval(target, frame);
			Seal f = field.seal();
			return doAssignToField(syn, value, A, f);

		} else if (asst instanceof AssignToSubscripted) {
			AssignToSubscripted atss = (AssignToSubscripted) asst;
			String operation = "():=";
			Cmd array = atss.array;
			List<Cmd> subscripts2 = atss.subscripts;
			return doAssignToSubscriptyThing(syn, frame, value, operation, array, subscripts2);
		} else if (asst instanceof AssignToMap) {
			AssignToMap atss = (AssignToMap) asst;
			String operation = "[]:=";
			Cmd array = atss.map;
			List<Cmd> subscripts2 = atss.subscripts;
			return doAssignToSubscriptyThing(syn, frame, value, operation, array, subscripts2);
		} else if (asst instanceof AssignToFieldOfSubscripted) {
			// This code loosely parallels code in visit(OpABExp
			AssignToFieldOfSubscripted afs = (AssignToFieldOfSubscripted) asst;
			Cmd array = afs.array;
			List<Cmd> subscripts2 = afs.subscripts;
			Thing[] subscripts = evaller.evalAll(subscripts2, frame);
			Id field = afs.field;
			Thing A = evaller.eval(array, frame);
			if (A instanceof TableTh) {
				TableTh table = (TableTh) A;
				table.mutateField(subscripts, field.str(), value, syn, 0, false);
				return EvalUtil.nip(syn);
			} else {
				// A is not a table.  Apply it to the arguments, as per function application.
				Thing Asub = doFunCall(syn, frame, A, subscripts);
				return doAssignToField(syn, value, Asub, field.seal());
			}
		} else {

			Doom.notYet();
			return null;
		}
	}

	private Thing doAssignToSubscriptyThing(Assign syn, Framelike frame, Thing value, String operation, Cmd array,
			List<Cmd> subscripts2) throws FisherException {
		Thing A = evaller.eval(array, frame);
		Thing[] subscripts = evaller.evalAllWithExtraSpace(subscripts2, frame, 1);
		subscripts[subscripts.length - 1] = value;
		Thing res1 = A.invokeMethod(operation, subscripts,  syn);
		Thing res = res1;
		return res;
	}

	private Thing doAssignToField(Assign syn, Thing value, Thing A, Seal f) throws FisherException, ThornThrow {
		if (A instanceof ObjectTh) {
			ObjectTh FA = (ObjectTh) A;
			FA.storeIntoVarField(f, value, syn);
			return value;
		} else if (A instanceof ModuleDynamicTh) {
			ModuleDynamicTh mod = (ModuleDynamicTh) A;
			//Seal sealFromModule = mod.moduleStatic.findSealOrNull(f.str());
			//sealFromModule.check(mod, value, syn);
			VarCell vc = mod.LValueByName(f.str(), syn);
			vc.setCargo(value, syn);
			return value;
		} else {
			Doom.throwy("Can't assign to fields of this.", syn);
			return null;
		}
	}

	@Override
	public Thing visit(Bind syn, Framelike frame) throws FisherException {
		return doBind(syn, frame, evaller);
	}

	public static Thing doBind(Bind syn, Framelike framelike, Evaller evaller) throws FisherException {
		if (syn.pat instanceof PatVar) {
			PatVar pv = (PatVar) syn.pat;
			Seal seal = pv.id.seal();
			Thing value = syn.exp == null ? Thing.NULL : evaller.eval(syn.exp, framelike);
			// Special case of filling in an uninitialized field of a class
			// inside a ctor:
			// class C{val v; new C(x){v=x;}} }
			if (syn.reallyAnInitializingAssignmentToAValField) {
				if (seal instanceof SealForVal) {
					SealForVal s4v = (SealForVal) seal;
					if (s4v.needsInit) {
						// syn.exp == null --- when it's the 'val v;' half.
						// syn.exp != null --- when it's the 'v = x;' half
						ObjectTh object = framelike.theThis(evaller, syn);
						object.store(s4v, value, syn);
						return EvalUtil.nip(syn);
					} else {
						Doom.internal("Attempt to bind a val that doesn't need initialization", syn, s4v, framelike);
						return null;
					}
				}
				assert (false);
			}
			// The case that happens almost all the time.
			framelike.store(seal, value, syn);
			return value;
		} else if (syn.exp == null) {
			// A special case for "val x : int;" in a class body.
			return null;
		} else {
			Thing subject = evaller.eval(syn.exp, framelike);
			boolean match = Matchiste.match(syn.pat, subject, framelike);
			if (!match) {
				Doom.runtime("Pattern doesn't match: " + syn, syn, "\nSubject=" + subject, framelike);
			}
			return subject;
		}
	}

	@Override
	public Thing visit(ImportStmt syn, Framelike frame) throws FisherException {
		Thing res = doImport(syn, frame);
		return res;
	}

	public static Thing doImport(ImportStmt syn, Framelike frame) throws FisherException {
		Thing res = syn.importage.eval(frame, Evaller.mine());
		return res;
	}

	@Override
	public Thing visit(Seq syn, Framelike frame) throws FisherException {
		Thing res = this.visitEach(syn.cmds, frame);
		return res;
	}
	
	@Override
	public Thing visit(Signature syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.body, arg);
	}

	@Override
	public Thing visit(Parens syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.exp, arg);
	}

	@Override
	public Thing visit(VarExp syn, Framelike frame) throws FisherException {
		Thing res = frame.RValue(syn.id.seal(), syn);
		return res;
	}

	@Override
	public Thing visit(OpExp syn, Framelike frame) throws FisherException {
		Thing res = syn.op.eval(syn, frame, evaller);
		return res;
	}

	@Override
	public Thing visit(AnonFun syn, Framelike frame) throws FisherException {
		ClosureTh closure = new ClosureTh(frame, syn.fun);
		return closure;
	}

	@Override
	public Thing visit(FunDecl syn, Framelike frame) throws FisherException {
		doFunDecl(syn, frame, evaller);
		return EvalUtil.nip(syn);
	}

	public static void doFunDecl(FunDecl syn, Framelike framelike, Evaller evaller) throws FisherException {
		ClosureTh closure = new ClosureTh(framelike, syn.funbody);
		framelike.store(syn.name.seal(), closure, syn);
	}

	@Override
	public Thing visit(FunCall syn, Framelike frame) throws FisherException {
		Thing f = evaller.eval(syn.function, frame);
		Thing[] args = evaller.evalAll(syn.args, frame);
		return doFunCall(syn, frame, f, args);
	}

	@Override
	public Thing visit(BracketCall syn, Framelike frame) throws FisherException {
		Thing f = evaller.eval(syn.function, frame);
		if (f == null) {
			Doom.runtime("Can't invoke methods on null", syn);
		}
		Thing[] args = evaller.evalAll(syn.args, frame);
		Thing result = f.invokeMethod("[]", args,  syn);
		return result;
	}

	private Thing doFunCall(Cmd syn, Framelike frame, Thing f, Thing[] args) throws FisherException {
		if (f instanceof Applyable) {
			Applyable applyable = (Applyable) f;
			try {
				Thing res = applyable.apply(args, frame, evaller, syn);
				return res;
			} catch (FisherReturn fret) {
				return fret.thing;
			}
		} else if (f instanceof ListTh) {
			ListTh L = (ListTh) f;
			if (args.length != 1) {
				return Doom.notYet();
			} else {
				// 1 arg
				if (args[0].isLong()) {
					long l = args[0].asLong(syn);
					return L.sub(l, syn);
				} else {
					return Doom.notYet();
				}
			}
		} else {
			return EvalUtil.methodCall(f, "()", args,  syn);
		}
	}

	@Override
	public Thing visit(ListCtor syn, Framelike frame) throws FisherException {
		ListTh L = ListTh.EMPTY;
		// Do the bits backwards, so the consing is right.
		for (int i = syn.bits.size() - 1; i >= 0; i--) {
			ListBit bit = syn.bits.get(i);
			if (bit instanceof ListBitExp) {
				ListBitExp lbe = (ListBitExp) bit;
				Thing thing = evaller.eval(lbe.exp, frame);
				L = L.cons(thing);
			} else if (bit instanceof ListBitEllip) {
				ListBitEllip lbe = (ListBitEllip) bit;
				Thing thing = evaller.eval(lbe.exp, frame);
				L = L.appendBehind(thing.asList(bit));
			} else {
				Doom.internal("Unknown or unaccounted kind of listbit!", syn, bit, evaller, frame);
				return null;
			}
		}

		// for (ListBit bit : syn.bits) {
		// if (bit instanceof ListBitExp) {
		// ListBitExp lbe = (ListBitExp) bit;
		// Thing thing = evaller.eval(lbe.exp, frame);
		// L.implAdd(thing);
		// } else if (bit instanceof ListBitEllip) {
		// ListBitEllip lbe = (ListBitEllip) bit;
		// Thing thing = evaller.eval(lbe.exp, frame);
		// L.implAddAll(thing.asList(bit));
		// } else {
		// Doom.internal("Unknown or unaccounted kind of listbit!", syn, bit,
		// evaller, frame);
		// return null;
		// }
		// }
		return L;
	}

	@Override
	public Thing visit(ClsDecl syn, Framelike frame) throws FisherException {
		return doClsDecl(syn, frame);
	}

	public final static Thing[] NO_ARGS = {};

	@Override
	public Thing visit(AnonObj syn, Framelike frame) throws FisherException {
		//		ClassDynamicTh dyn = doClsDecl(syn, frame);
		//		Thing instance = dyn.apply(NO_ARGS, frame, evaller, syn);
		//		return instance;
		return evaller.eval(syn.actualCode, frame);
	}

	public static ClassDynamicTh doClsDecl(Classlike syn, Framelike frame) throws FisherException {
		ClassDynamicTh result = ClassDynamizer.instantiate(syn, frame);
		Id name = syn.name();
		if (name != null)
			frame.store(name.seal(), result, (Syntax) syn);
		return result;
	}

	@Override
	public Thing visit(FieldRef syn, Framelike frame) throws FisherException {
		// X.y
		Thing X = evaller.eval(syn.target, frame);
		if (X == null) {
			Doom.throwy("null pointer", syn);
		}
		String y = syn.field.str();
		if (X instanceof ObjectTh) {
			ObjectTh O = (ObjectTh) X;
			return O.invokeMethod(y, NO_ARGS,   syn);
		} else if (X instanceof Fieldiferous) {
			Fieldiferous FX = (Fieldiferous) X;
			return FX.getField(y, syn);
		} else {
			return EvalUtil.methodCall(X, syn.field, NO_ARGS,   syn);
		}
	}

	@Override
	public Thing visit(MethodCall syn, Framelike frame) throws FisherException {
		Thing receiver = evaller.eval(syn.target, frame);
		try {
			Thing[] args = evaller.evalAll(syn.args, frame);
			return EvalUtil.methodCall(receiver, syn.method, args,  syn);
		} catch (FisherReturn fret) {
			return fret.thing;
		}
	}

	@Override
	public Thing visit(This syn, Framelike arg) throws FisherException {
		return arg.theThis(evaller, syn);
	}

	@Override
	public Thing visit(Valof syn, Framelike frame) throws FisherException {
		Framelike frame2 = syn.innerFrame ? Frame.inner(frame) : frame;
		return visitEach(syn.stmts, frame2);
	}

	@Override
	public Thing visit(Comparison syn, Framelike frame) throws FisherException {
		Thing left = evaller.eval(syn.first, frame);
		for (ComparisonBit bit : syn.rest) {
			Thing right = evaller.eval(bit.to, frame);
			ComparisonOp comparison = bit.comparison;
			boolean cf = comparison.compare(left, right, syn, frame, evaller);
			if (cf == false)
				return BoolTh.False;
			left = right;
		}
		return BoolTh.True;
	}

	@Override
	public Thing visit(While syn, Framelike frame) throws FisherException {
		Thing res = null;
		Seal thisLoopsSeal = syn.loopControlSeal;
		assert (thisLoopsSeal != null);
		try {
			if (syn.reallyDo) {
				try {
					res = evaller.eval(syn.body, frame);
				} catch (FisherContinueException loopContinue) {
					if (loopContinue.seeking != thisLoopsSeal)
						throw loopContinue;
					// If it *is* continue, fall through to the Java-while.
				}
			}
			while (evaller.eval(syn.test, frame).asBoolean(syn) != syn.reallyUntil) {
				try {
					res = evaller.eval(syn.body, frame);
				} catch (FisherContinueException loopContinue) {
					if (loopContinue.seeking != thisLoopsSeal)
						throw loopContinue;
					// if it *is* this loop's continue, fall through, thus
					// going back to this loop's while.
				}
			}
		} catch (FisherBreakException loopBreak) {
			if (loopBreak.seeking != thisLoopsSeal)
				throw loopBreak;
			// otherwise, break.
		}
		return res;
	}

	@Override
	public Thing visit(For syn, Framelike frame) throws FisherException {
		Thing res = null;
		Seal thisLoopsSeal = syn.loopControlSeal;
		Thing list = evaller.eval(syn.list, frame);
		try {
			thisForLoop: 
				for (Thing thing : EvalUtil.iter(list, syn)) {
					try {
					boolean match = Matchiste.match(syn.pat, thing, frame);
					if (match) {
						res = evaller.eval(syn.body, frame);
					} else if (syn.inquisitive) {
						// silently skip that iteration.
					} else {
						Doom.runtime("Match failed in for loop", syn, thing, syn.pat, syn, frame);
					}
					}
					catch(FisherContinueException loopContinue) {
						if (loopContinue.seeking != thisLoopsSeal) 
							throw loopContinue;
						else 
							continue thisForLoop;
					}
				}
			
		} catch (FisherBreakException loopBreak) {
			if (loopBreak.seeking != thisLoopsSeal)
				throw loopBreak;
			// otherwise, break.
		}
		return res;
	}

	@Override
	public Thing visit(Break syn, Framelike arg) throws FisherException {
		Seal loopSealedBy = syn.loopSeal;
		throw new FisherBreakException(loopSealedBy);
	}

	@Override
	public Thing visit(Continue syn, Framelike arg) throws FisherException {
		Seal loopSealedBy = syn.loopSeal;
		throw new FisherContinueException(loopSealedBy);
	}

	@Override
	public Thing visit(If syn, Framelike frame) throws FisherException {
		Thing test = evaller.eval(syn.test, frame);
		if (test.asBoolean(syn) != syn.reallyUnless) {
			return evaller.eval(syn.Then, frame);
		} else if (syn.Else != null) {
			return evaller.eval(syn.Else, frame);
		} else {
			return null;
		}
	}

	@Override
	public Thing visit(MatchExp syn, Framelike frame) throws FisherException {
		Thing subject = evaller.eval(syn.subject, frame);
		boolean matched = Matchiste.match(syn.pat, subject, frame);
		return BoolTh.of(matched);
	}

	@Override
	public Thing visit(ItExp syn, Framelike frame) throws FisherException {
		return frame.getIt(syn);
	}

	@Override
	public Thing visit(Match syn, Framelike frame) throws FisherException {
		Thing subject = evaller.eval(syn.subject, frame);
		List<Case> cases = syn.cases;
		// There is very similar code in Try (for catch clauses).
		for (Case cas : cases) {
			if (Matchiste.match(cas.pat, subject, frame)) {
				Thing result = evaller.eval(cas.body, frame);
				return result;
			}
		}
		Doom.runtime("No clause matches", syn, subject);
		return null;
	}

	@Override
	public Thing visit(Return syn, Framelike frame) throws FisherException {
		Thing v = syn.exp == null ? null : evaller.eval(syn.exp, frame);
		throw new FisherReturn(v, syn);
	}

	@Override
	public Thing visit(Throw syn, Framelike frame) throws FisherException {
		Thing exn = evaller.eval(syn.exn, frame);
		throw new ThornThrow(exn, syn);
	}

	@Override
	public Thing visit(Try syn, Framelike frame) throws FisherException {
		Thing res;
		try {
			res = evaller.eval(syn.body, frame);
			return res;
		} catch (ThornThrow tt) {
			// There is very similar code in Match
			// This is different, because it does something different if the
			// match fails.
			Thing subject = tt.thrown;
			for (Case cas : syn.cases) {
				if (Matchiste.match(cas.pat, subject, frame)) {
					Thing result = evaller.eval(cas.body, frame);
					return result;
				}
			}
			// None matched, so ... rethrow!
			throw tt;
		} finally {
			if (syn.fin != null)
				evaller.eval(syn.fin, frame);
		}
	}

	@Override
	public Thing visit(RecordCtor syn, Framelike frame) throws FisherException {
		RecordTh ret = RecordTh.construct(syn.fields, frame);
		return ret;
	}

	@Override
	public Thing visit(StringWithInterpolations syn, Framelike arg) throws FisherException {
		StringBuffer sb = new StringBuffer();
		for (AbstractStringBit bit : syn.bits) {
			if (bit instanceof StringBitText) {
				StringBitText txt = (StringBitText) bit;
				sb.append(txt.value);
			} else if (bit instanceof StringBitVar) {
				StringBitVar sbv = (StringBitVar) bit;
				Thing thing = evaller.eval(sbv.exp, arg);
				sb.append(thing + "");
			} else {
				Doom.internal("What is this stringbit " + bit.getClass(), syn, bit);
			}
		}
		return StringTh.of(sb.toString());
	}

	@Override
	public Thing visit(SuperCall syn, Framelike frame) throws FisherException {
		// The code of visit(SuperCtorCall is very similar!
		Seal sealForSuperclass = syn.sealForSuperclass;
		Thing supposedSuperclass = frame.RValue(sealForSuperclass, syn);
		if (supposedSuperclass instanceof ClassDynamicTh) {
			ClassDynamicTh superclass = (ClassDynamicTh) supposedSuperclass;
			MethodSig sig = syn.sig;
			MethDecl methDecl = superclass.classStatic.findMethodOrNull(sig);
			if (methDecl == null) {
				return Doom.runtime("Superclass doesn't have have the method!", syn, "superclass", superclass,
						"method name", syn.method);
			}
			Thing[] args = evaller.evalAll(syn.args, frame);
			ObjectTh receiver = frame.theThis(evaller, syn);
			Thing superRes = receiver.invokeMethDecl(methDecl, args,   syn);
			return superRes;
		} else {
			return Doom.runtime("Supposed superclass is not a class.", syn, "supposed superclass", supposedSuperclass,
					supposedSuperclass.typeString());
		}
		// End of echoing code.
	}

	@Override
	public Thing visit(SuperCtorCall syn, Framelike frame) throws FisherException {
		if (syn.isCallToADifferentCtorForThis()) {
			// new(a,b,c) -- which is NOT a supercall, it's a call to a constructor of the current class.
			// And basically like a function call.
			int arity = syn.args.size();
			ClassStatic thisClassStatic = syn.classStaticForCurrentClass;
			ClassDynamicTh thisClassDynamic = frame.theThis(evaller, syn).classDynamic;
			ClsCtorDef ccd = thisClassStatic.findCtorOrNull(arity);
			if (ccd == null) {
				return Doom.runtime("Class doesn't have have a " + arity + "-ary constructor", syn, "class",
						thisClassStatic);
			}
			Thing[] args = evaller.evalAll(syn.args, frame);
			ObjectTh receiver = frame.theThis(evaller, syn);
			thisClassDynamic.invokeConstructorForSupercall(receiver, ccd, args, evaller, frame, syn);
			return receiver;

		} else {
			// The code of visit(SuperCall is very similar!
			Seal sealForSuperclass = syn.sealForSuperclass;
			Thing supposedSuperclass = frame.RValue(sealForSuperclass, syn);
			if (supposedSuperclass instanceof ClassDynamicTh) {
				ClassDynamicTh superclass = (ClassDynamicTh) supposedSuperclass;
				int arity = syn.args.size();

				if (arity == 0 && superclass.classStatic.constructors.isEmpty()) {
					// Special case -- A() is allowed for a class A without ctors.
					return frame.theThis(evaller, syn);
				}

				ClsCtorDef ccd = superclass.classStatic.findCtorOrNull(arity);

				if (ccd == null) {
					return Doom.runtime("Superclass doesn't have have a " + arity + "-ary constructor", syn,
							"superclass", superclass);
				}
				Thing[] args = evaller.evalAll(syn.args, frame);
				ObjectTh receiver = frame.theThis(evaller, syn);
				superclass.invokeConstructorForSupercall(receiver, ccd, args, evaller, frame, syn);
				return receiver;
			} else {
				return Doom.runtime("Supposed superclass is not a class.", syn, "supposed superclass",
						supposedSuperclass, supposedSuperclass.typeString());
			}
			// End of echoing code.
		}
	}

	@Override
	public Thing visit(QueryQuantifierEvery syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryQuantifierSome syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryQuantifierCount syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryFirst syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryAfter syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryListComprehension syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QuerySort syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryTable syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(Table syn, Framelike frame) throws FisherException {
		return new TableTh(syn.colInfos, syn.keyInfos, syn.nonkeyInfos, syn.mapColInfo, syn, frame);
	}

	@Override
	public Thing visit(Ord syn, Framelike frame) throws FisherException {
		return new OrdTh(syn.colInfos, syn.keyInfos, syn.nonkeyInfos, syn.mapColInfo, syn, frame);
	}

	@Override
	public Thing visit(MapCtor syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(QueryGroup syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(Spawn syn, Framelike frame) throws FisherException {
		SiteData currentSite = SiteData.mine();
		ComponentThread ct = currentSite.spawn(syn, frame);
		return ct.compTh;
	}

	@Override
	public Thing visit(Send syn, Framelike frame) throws FisherException {

		Thing threc = evaller.eval(syn.receiver, frame);
		Thing thmsg = evaller.eval(syn.exp, frame);
		Thing security = syn.security == null ? null : evaller.eval(syn.security, frame);
		TestUtils.say(TestUtils.printSend, ComponentThread.mine(), " about to call <<<: " + syn + " to " + threc + ":"+ EvalUtil.kind(threc)+ " msg " + thmsg);
		Thing result = EvalUtil.methodCall(threc, "<<<", Bard.array(thmsg, security),  syn);
		return result;
		//DistUtil.send(ComponentTh.mine(syn), threc, thmsg, syn);
		//return BoolTh.True;
	}

	@Override
	public Thing visit(Recv syn, Framelike frame) throws FisherException {
		Thing recvd = ComponentThread.mine().recv(syn, frame);
		TestUtils.say(TestUtils.recv, ComponentThread.mine(), "received " + recvd);
		return recvd;
	}

	@Override
	public Thing visit(AsyncStmt syn, Framelike arg) throws FisherException {
		TestUtils.say(TestUtils.asyncStmt, ComponentThread.mine(), "starting async stmt: " + syn);
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(SyncStmt syn, Framelike arg) throws FisherException {
		TestUtils.say(TestUtils.syncStmt, ComponentThread.mine(), "starting sync stmt: " + syn);
		Thing ret = evaller.eval(syn.actualCode, arg);
		TestUtils.say(TestUtils.syncStmt, ComponentThread.mine(), "ending sync stmt: " + syn);
		return ret;
	}

	@Override
	public Thing visit(Serve syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	@Override
	public Thing visit(JavalyFun syn, Framelike frame) throws FisherException {
		return doJavalyFunDecl(syn, frame);
	}

	public static Thing doJavalyFunDecl(JavalyFun syn, Framelike frame) throws FisherException {
		JavalyFunImpl javaly = new JavalyFunImpl(syn.method, syn);
		frame.store(syn.name.seal(), javaly, syn);
		return javaly;
	}

	public static Thing doJavalyClassDecl(JavalyClassDecl syn, Framelike frame) throws FisherException {
		ExtensionClass ec = new ExtensionClass(syn.cls, syn.name, syn.fields);
		frame.store(syn.name.seal(), ec, syn);
		return ec;
	}

	@Override
	public Thing visit(TypedExp syn, Framelike frame) throws FisherException {
		Thing thing = evaller.eval(syn.exp, frame);
		Seal seal = syn.type.last().seal();
		final Thing type = frame.RValue(seal, syn);
		if (EvalUtil.thingIsType(thing, type, syn)) {
			return thing;
		} else {
			Doom.runtime("Type constraint is violated. ", syn, syn.exp + " = " + thing + " which is a "
					+ EvalUtil.kind(thing), syn.type + " = " + type);
			return null;
		}
	}

	@Override
	public Thing visit(CmdsInAList syn, Framelike arg) throws FisherException {
		Thing res = null;
		for (Cmd cmd : syn.cmds) {
			res = evaller.eval(cmd, arg);
		}
		return res;
	}

	@Override
	public Thing visit(ComponentDecl syn, Framelike arg) throws FisherException {
		return evaller.eval(syn.actualCode, arg);
	}

	private final static String cccomponentc = ComponentDeclDesugarer.CCcomponentC.str();

	@Override
	public Thing visit(SpawnByComponentName syn, Framelike frame) throws FisherException {
		Thing compdecl = evaller.eval(syn.exp, frame);
		// compdecl must have the form {: ©©component©: fn () => spawn... :}
		if (compdecl instanceof RecordTh) {
			RecordTh rec = (RecordTh) compdecl;

			if (rec.hasField(cccomponentc)) {
				Thing fn = rec.getField(cccomponentc, syn);
				if (fn instanceof ClosureTh) {
					ClosureTh clo = (ClosureTh) fn;
					Thing[] args = evaller.evalAll(syn.args, frame);
					Thing res = clo.apply(args, frame, evaller, syn);
					return res;
				}
			}
		} else {
		} /**/
		Doom.runtime("Invalid component to spawn: " + compdecl, syn);
		return null;
	}

}
