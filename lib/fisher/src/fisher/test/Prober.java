
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.ListTh;
import fisher.runtime.ObjectTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.Letter;
import fisher.statics.Env;
import fisher.statics.Seal;
import fisher.statics.SealMaker;
import fisher.statics.Sealant;
import fisher.syn.Cmd;
import fisher.syn.Probe;
import fisher.syn.QueryAbstract;
import fisher.syn.VarExp;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherRuntimeException;

public enum Prober {
	NULL("") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Doom.runtime("The null probe is not really defined yet.", syn);
			return null;
		}
	},
	ASSERT("assert"){
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			Cmd cmd = syn.exps.get(0);
				if(!values[0].isBoolean()) {
					Doom.testFailure("Assertion is not even boolean", cmd, values[0]);
				}
				else if (!values[0].asBoolean(cmd)) {
					Doom.testFailure("Assertion failed", cmd, values);
				}
			return null;
		}
	},
	HIT("hit") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			return null;
		}
	},
	BAD("bad") {
		// Any sort of exception
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			for (Cmd cmd : syn.exps) {
				Thing value = null;
				boolean badAsDesired;
				try {
					value = evaller.eval(cmd, frame);
					badAsDesired = false;
				} catch (Exception ex) {
					// It is desired that it throws an exception,
					// So this is the *good* case;
					badAsDesired = true;
				}
				catch (Throwable ex) {
					// It is desired that it throws an exception,
					// So this is the *good* case;
					badAsDesired = true;
				}
				if (!badAsDesired) {
					Doom.testFailure("Failure of ~!@bad.  " + cmd + " evaluated to " + value
							+ "\n(but it was supposed to cause an error.)", syn, cmd, value, frame, evaller);
				}
			}
			return null;
		}
	},
	EXN("exn") {
		// FisherException
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			for (Cmd cmd : syn.exps) {
				Thing value = null;
				boolean badAsDesired;
				try {
					value = evaller.eval(cmd, frame);
					badAsDesired = false;
				} catch (FisherException ex) {
					// It is desired that it throws an exception,
					// So this is the *good* case;
					badAsDesired = true;
				}
				
				if (!badAsDesired) {
					Doom.testFailure("Failure of ~!@bad.  " + cmd + " evaluated to " + value
							+ "\n(but it was supposed to cause an error.)", syn, cmd, value, frame, evaller);
				}
			}
			return null;
		}
	},
	SAME_PTR("same_ptr") {
		// Compare all pairs of arguments for equality.
		// (Which is to say, ~!@eq(x) could fail, if, somehow, x =/= x.)
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			for (int i = 0; i < values.length; i++) {
				Thing thingi = values[i];
				for (int j = 0; j < values.length; j++) {
					Thing thingj = values[j];
					if ((thingi != thingj)) {
						Doom.testFailure("Failure of ~!@same_ptr\nthing " + i + " = " + syn.exps.get(i) + " = " + thingi
								+ "\nthing " + j + " = " + syn.exps.get(j) + " = " + thingj
								+ "\n(and they are different).", syn, thingi, thingj, frame, evaller);
						return null;
					}
				}
			}
			return EvalUtil.nip(syn);
		}
	},
	EQPRINT("eqprint") {
		// Compare all pairs of arguments for equality.
		// (Which is to say, ~!@eq(x) could fail, if, somehow, x =/= x.)
		// Except last arg, which is just a clue that goes into the error message.
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			for (int i = 0; i < values.length-1; i++) {
				Thing thingi = values[i];
				for (int j = 0; j < values.length-1; j++) {
					Thing thingj = values[j];
					if (!EvalUtil.eq(thingi, thingj)) {
						Thing clue = values[values.length-1];
						Doom.testFailure("Failure of ~!@eq\nthing " + i + " = " + syn.exps.get(i) + " = " + thingi
								+ "\nthing " + j + " = " + syn.exps.get(j) + " = " + thingj
								+ "\n(and they are different)."
								+ "\nClue = " + syn.exps.get(values.length-1) + " = " + clue
								, syn, thingi, thingj, frame, evaller);
						return null;
					}
				}
			}
			return EvalUtil.nip(syn);
		}
	},
	EQ("eq") {
		// Compare all pairs of arguments for equality.
		// (Which is to say, ~!@eq(x) could fail, if, somehow, x =/= x.)
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			for (int i = 0; i < values.length; i++) {
				Thing thingi = values[i];
				for (int j = 0; j < values.length; j++) {
					Thing thingj = values[j];
					if (!EvalUtil.eq(thingi, thingj)) {
						Doom.testFailure("Failure of ~!@eq\nthing " + i + " = " + syn.exps.get(i) + " = " + thingi
								+ "\nthing " + j + " = " + syn.exps.get(j) + " = " + thingj
								+ "\n(and they are different).", syn, thingi, thingj, frame, evaller);
						return null;
					}
				}
			}
			return EvalUtil.nip(syn);
		}
	},
	QEQ("qeq") {
		// Like eq, but if it fails, prints the expansion of all queries in its list.
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			for (int i = 0; i < values.length; i++) {
				Thing thingi = values[i];
				for (int j = 0; j < values.length; j++) {
					Thing thingj = values[j];
					if (!EvalUtil.eq(thingi, thingj)) {
						String expansions = "";
						for (int k = 0; k < values.length; k++) {
							Cmd expk = syn.exps.get(k);
							if (expk instanceof QueryAbstract) {
								QueryAbstract qa = (QueryAbstract) expk;
								expansions = expansions + "\n" + qa.actualCode + "\n";
							}
						}
						Doom.testFailure("Failure of ~!@eq\nthing " + i + " = " + syn.exps.get(i) + " = " + thingi
								+ "\nthing " + j + " = " + syn.exps.get(j) + " = " + thingj
								+ "\n(and they are different).\n\n\n" + expansions + "\n\n\n" , syn, thingi, thingj, frame, evaller);
						
						return null;
					}
				}
			}
			return EvalUtil.nip(syn);
		}
	},
	PRQ("prq") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			for (Syntax qa : syn.exps) {
				if (qa instanceof QueryAbstract) {
					QueryAbstract qaa = (QueryAbstract) qa;
					System.out.println(qaa.toString());
				}
			}
			return null;
		}
	},
	OBJ_LOCAL_STRUCT("obj_local_struct") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			// list the local fields of arg[0] (an ObjectTh) into a string,
			// format a=1,b=2,c=3,  (note trailing comma)
			// (alphabetical order)
			// That had better be equal to the other argument.
			Thing[] values = evaller.evalAll(syn.exps, frame);
			ObjectTh obj = (ObjectTh) values[0];
			StringTh str = (StringTh) values[1];
			Set<String> keySet = obj.fields.keySet();
			String[] keyArr = sortedByString(keySet);
			StringBuffer sb = new StringBuffer();
			for (String key : keyArr) {
				Thing fld = obj.getField(key,  syn);
				sb.append(key + "=" + fld + ",");
			}
			String s1 = sb.toString();
			String s2 = str.value;
			if (!(s1.equals(s2))) {
				Doom.testFailure("Failure of ~obj_local_struct for " + obj, syn, "got  = " + s1, "want =" + s2);
			}
			return null;
		}
	},
	NE("ne") {
		// Compare all pairs of arguments for non-equality.
		// (Which is to say, ~!@eq(x) could fail, if, somehow, x =/= x.)
		// arg[i] should equal arg[j] precisely if i==j.
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			for (int i = 0; i < values.length; i++) {
				Thing thingi = values[i];
				for (int j = 0; j < values.length; j++) {
					Thing thingj = values[j];
					boolean desiredEqualityValue = (i == j);
					if (EvalUtil.eq(thingi, thingj) != desiredEqualityValue) {
						String msg = "Failure of ~!@ne\nthing " + i + " = " + syn.exps.get(i) + " = " + thingi
								+ "\nthing " + j + " = " + syn.exps.get(j) + " = " + thingj + "\n(and they are "
								+ (desiredEqualityValue ? "different" : "equal") + ".)\n";
						Doom.testFailure(msg, syn, thingi, thingj, frame, evaller);
						return null;
					}
				}
			}
			return EvalUtil.nip(syn);
		}
	},
	UNDEF("undef") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			List<String> idNamesWhichAreDefinedAndShouldNotBe = (List<String>) syn.scratch;
			if (! idNamesWhichAreDefinedAndShouldNotBe.isEmpty()) {
				Doom.testFailure("Some identifiers are defined and should not be: "
						+ Bard.sep(idNamesWhichAreDefinedAndShouldNotBe, ","), syn, frame, evaller);
			}
			return EvalUtil.nip(syn);
		}

		// ~!@undef(a,b,c) produces an error if any identifier a, b, or c has a
		// definition.
		// Internally, during the sealant phase, it writes down the answer on
		// its 'scratch' field.
		@Override
		public Env seal(Probe syn, Env env, Sealant sealant) throws FisherException {
			return computeDefinedness(syn, env, false);
		}

	}, // UNDEF
	DEF("isdef") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			List<String> idNamesWhichAreNotDefinedAndShouldBe = (List<String>) syn.scratch;
			if (! idNamesWhichAreNotDefinedAndShouldBe.isEmpty()) {
				Doom.testFailure("Some identifiers are not defined and should be: "
						+ Bard.sep(idNamesWhichAreNotDefinedAndShouldBe, ","), syn, frame, evaller);
			}
			return EvalUtil.nip(syn);
		}

		@Override
		public Env seal(Probe syn, Env env, Sealant sealant) throws FisherException {
			return computeDefinedness(syn, env, true);
		}
	},
	BREAKER("breaker") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			return null;
		}
	},
	KIND("kind") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			assert(syn.exps.size() == 1);
			Thing v = evaller.eval(syn.exps.get(0), frame);
			return EvalUtil.kind(v);
		}
	},
	IMM("immutable?") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			assert(syn.exps.size() == 1);
			Thing v = evaller.eval(syn.exps.get(0), frame);
			return BoolTh.of(EvalUtil.isImmutable(v, new ImmutabilityContext()));
		}
	},
	TESTSTARTLATCH("testStartLatch") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			try {
				TestUtils.testStartLatch.await();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return null;
		}
	},
	PRINTLN("println") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			for (Thing thing : values) {
				System.out.print(thing+"");
			}
			System.out.println("");
			System.out.flush();
			return null;
		}
	},
	PHASE("phase"){
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			assert(values.length == 2);
			TestUtils.phase(values[0], values[1]);
			return null;
		}
	}
	, 
	CHECKPHASE("checkphase") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] valuesA = evaller.evalAll(syn.exps, frame);
			List<Thing> values = Bard.list();
			for (Thing t : valuesA) {
				if (t instanceof ListTh) {
					ListTh tl = (ListTh) t;
					values.addAll(tl.flatten().toJavaList());
				}
				else {
					values.add(t);
				}
			}
			Set<Thing> actualPhases = TestUtils.phases();
			Set<Thing> actualPhasesThatNeedToBeConfirmed = new HashSet<Thing>(actualPhases);
			String reqdArgs = "~!@checkphase arguments must be {: phase: ph, bag: bag :}";
			if (values.size() != actualPhases.size()) {
				Doom.runtime("~!@checkphase: wanted " + values.size() + " phases, but got " + actualPhases.size(), syn, values, 
						"actual phases: {" + Bard.sep(actualPhases, ", ") + "}"
				);
			}
			for (Thing thing : values) {
				if (thing instanceof RecordTh) {
					RecordTh recth = (RecordTh) thing;
					Thing desiredPhase = null;
					if(recth.hasField("phase")) {
						desiredPhase = recth.getField("phase", syn);
					}
					else {
						Doom.runtime(reqdArgs, syn, thing);
					}
					Thing desiredBagTh = null;
					if(recth.hasField("bag")){
						desiredBagTh = recth.getField("bag", syn);
					}
					else {
						Doom.runtime(reqdArgs, syn, thing);
					}
					List<Thing> desiredBag = null;
					if (desiredBagTh instanceof ListTh) {
						desiredBag = ((ListTh)desiredBagTh).toJavaList();						
					}
					List<Thing> actualBag = TestUtils.phased(desiredPhase);
					if (actualBag == null) {
						Doom.runtime("A phase should have occurred for " + desiredPhase + ", but none did", syn, desiredPhase, desiredBagTh);
					}
					if (Bard.bagEq(desiredBag, actualBag)) {
						// Yay! 
						actualPhasesThatNeedToBeConfirmed.remove(desiredPhase);
					}
					else {
						Doom.runtime("The desired bag for phase " + desiredPhase + " did not match the actual one.\n"
								+ "Desired bag = " + Bard.sep(desiredBag, ", ") + "\n"
								+ "Actual  bag = " + Bard.sep(actualBag, ", ") + "\n",
								syn, desiredPhase, desiredBag, actualBag
						);
					}
				}
				else {
					Doom.runtime(reqdArgs, syn, thing);
				}
			}//for
			
			if (! actualPhasesThatNeedToBeConfirmed.isEmpty()) {
				Doom.runtime("Not all actual phases were confirmed: " + Bard.sep(actualPhasesThatNeedToBeConfirmed, ", "), syn);
			}
			
			return null;
		}
	},
	MAILBOX("mailbox"){
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			ComponentThread ct = ComponentThread.mine();
			List<Thing> L = Bard.list();
			for (Letter letter : ct.mailbox.queue) {
				L.add(letter.contents());
			}
			return ListTh.fromJavaList(L);
		}
	},
	SLEEP("sleep") {
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			try {
				Thread.sleep(values[0].asLong(syn));
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			return null;
		}
	}
	,
	LATCHMAKE("latchmake") {
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			assert(values.length == 2);
			Thing key = values[0];
			int count = (int) values[1].asLong(syn);
			assert(!TestUtils.latches.containsKey(key));
			TestUtils.latches.put(key, new CountDownLatch(count));
			return null;
		}
	},
	LATCHWAIT("latchwait") {
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			assert(values.length == 1);
			Thing key = values[0];
			assert(TestUtils.latches.containsKey(key));
			try {
				TestUtils.latches.get(key).await();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			return null;
		}
	},
	LATCHCOUNT("latchcount") {
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			Thing[] values = evaller.evalAll(syn.exps, frame);
			assert(values.length == 1);
			Thing key = values[0];
			assert(TestUtils.latches.containsKey(key));
			TestUtils.latches.get(key).countDown();
			return null;
		}
	},
	RECOFPORTS("recOfPorts"){
		@Override
		public Thing compute(Probe syn, Framelike frame, Evaller evaller) throws FisherException {
			return TestUtils.recOfPorts;
		}
	}
	
	;

	public Env seal(Probe syn, Env env, Sealant sealant) throws FisherException {
		return sealant.sealThus(env, syn.exps);
	}

	public abstract Thing compute(Probe syn, Framelike frame, fisher.eval.Evaller evaller)
			throws FisherException;

	public final String text;

	private Prober(String text) {
		this.text = text;
	}

	public static Prober of(String s) {
		for (Prober prober : Prober.values()) {
			if (s.equals(prober.text))
				return prober;
		}
		return null;
	}

	protected Env computeDefinedness(Probe syn, Env env, boolean desiredDefinedness) throws FisherException {
		List<String> defined = new ArrayList<String>();
		List<String> undefined = new ArrayList<String>();
		syn.scratch = desiredDefinedness ? undefined : defined;
		for (Cmd arg : syn.exps) {
			if (arg instanceof VarExp) {
				VarExp ve = (VarExp) arg;
				boolean def = env.hasSealFor(ve.id);
				(def ? defined : undefined).add(ve.id.str());
				// Mark all of the ids as "being_tested", regardless of
				// whether they are found or not, to avoid error messages.
				Seal seal = SealMaker.being_tested(arg, ve.toString());
				// Don't add that to the env!
				ve.id.setSeal(seal, syn);
			} else {
				syn.flag(DangerLevel.ERROR, "~!@undef only works on identifiers", "");
			}
		}
		return env;
	}

	public static String[] sortedByString(Collection<String> stuff) {
		String[] s = new String[stuff.size()];
		stuff.toArray(s);
		Arrays.sort(s);
		return s;
	}
	
}
