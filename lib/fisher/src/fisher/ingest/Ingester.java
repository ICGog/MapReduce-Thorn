
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.ingest;

import static fisher.util.DangerLevel.ERROR;
import static fisher.util.DangerLevel.INTERNAL_ERROR;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.desugar.DistDesugarer;
import fisher.eval.EvalUtil;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.statics.MethodSig;
import fisher.syn.AnonObj;
import fisher.syn.Assign;
import fisher.syn.Bind;
import fisher.syn.Case;
import fisher.syn.ClsCtorDef;
import fisher.syn.ClsDecl;
import fisher.syn.ClsExtends;
import fisher.syn.Cmd;
import fisher.syn.FunDecl;
import fisher.syn.HighLevelCommunication;
import fisher.syn.IdWithOptInit;
import fisher.syn.JavalyClassDecl;
import fisher.syn.JavalyFun;
import fisher.syn.JavalyMethodDecl;
import fisher.syn.JavalyNewDecl;
import fisher.syn.Literal;
import fisher.syn.Match;
import fisher.syn.MethDecl;
import fisher.syn.MethodCall;
import fisher.syn.OpExp;
import fisher.syn.PatLiteral;
import fisher.syn.PatTypeTest;
import fisher.syn.PatVar;
import fisher.syn.ProcBody;
import fisher.syn.ProcInit;
import fisher.syn.QualName;
import fisher.syn.QueryGroup;
import fisher.syn.QueryTable;
import fisher.syn.Recv;
import fisher.syn.Seq;
import fisher.syn.Serve;
import fisher.syn.Spawn;
import fisher.syn.SuperCall;
import fisher.syn.SuperCtorCall;
import fisher.syn.Table;
import fisher.syn.TableFields;
import fisher.syn.core.ColAccess;
import fisher.syn.core.ColSpecial;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ClassMember;
import fisher.syn.interfaces.ComponentInfo;
import fisher.syn.interfaces.LocalMember;
import fisher.syn.interfaces.ObjectMember;
import fisher.syn.interfaces.ProcMember;
import fisher.syn.interfaces.TableMember;
import fisher.syn.visitor.VanillaWalker;
import fisher.util.Bard;
import fisher.util.DangerLevel;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Ingester extends VanillaWalker<IngestionContext, FisherException>  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static final String A_SPAWN_CAN_HAVE_ONLY_ONE_INIT = "a 'spawn' can have only one 'init'";
	public static final String A_SPAWN_CAN_HAVE_ONLY_ONE_BODY = "a 'spawn' can have only one 'body'";
	public static final String A_SPAWN_MUST_HAVE_A_BODY_CLAUSE = "A 'spawn' must have a body clause";
	public static final String TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS = "Table fields cannot have initializers";
	public static final String TABLE_QUERY_FIELDS_NEED_INITIALIZERS = "Table query fields need initializers";
	public static final String A_CONSTRUCTOR_MUST_HAVE_THE_SAME_NAME_AS_THE_CLASS_IT_CONSTRUCTS = "A constructor must have the same name as the class it constructs.";
	public static final String ASSIGNMENT_LIST_MISMATCH = "Assignment list mismatch";
	public static final String ANONYMOUS_OBJECTS_CAN_T_HAVE_CONSTRUCTORS = "Anonymous objects can't have constructors";
	public static final String THIS_IS_A_PATTERN_BUT_NOT_AN_EXPRESSION = "This is a pattern, but not an expression";
	public static final String WRONG_NUMBER_OF_ARGS = "Wrong number of arguments";
	public static final String ONLY_ONE_MAP = "A table may have only one 'map' field";
	public static final String MAP_IS_NOT_KEY = "A field may not be both 'map' and 'key'";
	public static final String PRE_ELLIPSIS_ONLY_IN_GROUP = "The prefix '...' (as in '...x') is only allowed inside non-key initializers of a %group";
	public static final String FROM_IN_MATCH = "'from' is not allowed in match.";
	public static final String MATCH_PRIO_IN_ORDER = "The prio numbers in a match statement must be in decreasing (non-increasing) order.";
	public static final String CLSDECL_VAL_MATCH_MUST_HAVE_SUBJECT = "Uninitialized val fields of a class must be declared 'val x;', not with a pattern.";
	public static Ingester IT = new Ingester();

	private Ingester() {
	}

	/*
	 * The Ingester walks over a newly-parsed syntax tree, makes various simple syntactic changes, 
	 * and finds simple errors and flaws. 
	 */

	/**
	 * Check <code>syn</code> and all its children. This is the method you
	 * should be using.
	 * 
	 * @param syn
	 */
	public static void check(Syntax syn) throws FisherException {
		syn.accept(IT, null);
		for (Syntax child : syn.children()) {
			IT.check(child);
		}
	}

	@Override
	public void visit(Syntax syn, IngestionContext arg) throws FisherException {
		// Nothing to do here.
	}

	@Override
	public void visit(OpExp syn, IngestionContext arg) throws FisherException {
		if (syn.op.numberOfArgumentsIsRight(syn.operands)) {
			// all is well
		} else {
			syn.flag(INTERNAL_ERROR, WRONG_NUMBER_OF_ARGS, "");
		}
	}

	@Override
	public void visit(AnonObj syn, IngestionContext arg) throws FisherException {
		for (ObjectMember member : syn.members) {
			if (member instanceof ClsCtorDef) {
				member.flag(ERROR, ANONYMOUS_OBJECTS_CAN_T_HAVE_CONSTRUCTORS, "");
			} else if (member instanceof MethDecl) {
				MethDecl md = (MethDecl) member;
				md.origin = syn;
			}
		}
	}

	@Override
	public void visit(Assign syn, IngestionContext arg) throws FisherException {
		if (syn.lhs.size() != syn.rhs.size()) {
			syn.flag(ERROR, ASSIGNMENT_LIST_MISMATCH,
					"The left-hand side of an assignment must have the same number of entries as the right-hand side. This one has "
							+ +syn.lhs.size() + " on the left and " + syn.rhs.size() + " on the right.");
		}
	}

	@Override
	public void visit(ClsDecl syn, IngestionContext arg) throws FisherException {
		for (ClassMember member : syn.members) {
			if (member instanceof ClsCtorDef) {
				ClsCtorDef ctor = (ClsCtorDef) member;
				if (ctor.id == null || ctor.id.equals(syn.name)) {
					// All is well
				} else {
					member.flag(ERROR, A_CONSTRUCTOR_MUST_HAVE_THE_SAME_NAME_AS_THE_CLASS_IT_CONSTRUCTS, "");
				}
//				confirm_ctor_has_all_necessary_supercalls(syn, ctor, arg);
			} else if (member instanceof Bind) {
				Bind bind = (Bind) member;
				// Revising this to allow for "var x:int";
				//				if (bind.exp == null && !(bind.pat instanceof PatVar)) {
				//					member.flag(ERROR, CLSDECL_VAL_MATCH_MUST_HAVE_SUBJECT, "");
				//				}

				if (bind.exp == null) {
					boolean varAlone = (bind.pat instanceof PatVar);
					boolean varAndType = bind.pat instanceof PatTypeTest;
					if (!(varAlone || varAndType)) {
						member.flag(ERROR, CLSDECL_VAL_MATCH_MUST_HAVE_SUBJECT, "");
					}
				}
			} else if (member instanceof MethDecl) {
				MethDecl md = (MethDecl) member;
				md.origin = syn;
			}
		}
	}

	private static void confirm_ctor_has_all_necessary_supercalls(ClsDecl clsDecl, ClsCtorDef ctor, IngestionContext arg)
			throws FisherException {
		final Cmd body = ctor.monobody.body;
		List<Cmd> bod = body instanceof Seq ? ((Seq) body).cmds : Bard.list(body);

		Set<QualName> requiredSuperNames = Bard.set();

		final List<ClsExtends> exts = clsDecl.exts;
		for (ClsExtends clsExtends : exts) {
			final QualName superName = clsExtends.superName;
			requiredSuperNames.add(superName);
		}

		boolean hasNewThisCall = false;

		Set<QualName> actualSuperNames = new HashSet<QualName>();
		for (Cmd c : bod) {
			if (c instanceof SuperCtorCall) {
				SuperCtorCall scc = (SuperCtorCall) c;
				final QualName cls = scc.cls;
				if (cls == null)
					hasNewThisCall = true;
				else {
					actualSuperNames.add(cls);
				}
			} else {
				break;
			}
		}

		final boolean differentSuperCtorCalls = !(requiredSuperNames.equals(actualSuperNames));
		
		if (!hasNewThisCall && differentSuperCtorCalls) {

			ctor.flag(DangerLevel.ERROR, "Wrong collection of supercalls", "", "\n wanted: "
					+ Bard.sep(requiredSuperNames, ", "), "\n got   : " + Bard.sep(actualSuperNames, ", "), "\n class="
					+ clsDecl, "\n ctor=" + ctor);

		}
	}

	public void visit(fisher.syn.ForPatternOnly syn, IngestionContext arg) throws FisherException {
		syn.flag(ERROR, THIS_IS_A_PATTERN_BUT_NOT_AN_EXPRESSION, "");
	}

	@Override
	public void visit(TableFields syn, IngestionContext arg) throws FisherException {

		if (syn.colSpecial == ColSpecial.MAP && syn.idInits.size() != 1) {
			syn.flag(ERROR, ONLY_ONE_MAP, "A table may only have one map field. This one has several: " + syn);
		}

		if (syn.colSpecial == ColSpecial.MAP && syn.colAccess == ColAccess.KEY) {
			syn.flag(ERROR, MAP_IS_NOT_KEY, "", syn);
		}
		// TODO -- similar things for List.
	}

	@Override
	public void visit(Table syn, IngestionContext arg) throws FisherException {
		boolean fieldsShouldBeInitialized = false;
		checkInitsAndMaps(syn, fieldsShouldBeInitialized, syn.vals);
	}

	private void checkInitsAndMaps(Cmd syn, boolean fieldsShouldBeInitialized, List<? extends TableMember> items)
			throws FisherException {
		List<TableFields> maps = new ArrayList<TableFields>(0);
		for (TableMember item : items) {
			if (item instanceof TableFields) {
				TableFields fields = (TableFields) item;
				if (fields.colSpecial == ColSpecial.MAP)
					maps.add(fields);
				fieldsVsInitializers(fields, fieldsShouldBeInitialized);
			}
		}
		if (maps.size() > 1) {
			syn.flag(ERROR, ONLY_ONE_MAP, "A table may have, at most, one map field. This one has several: "
					+ Bard.sep(maps, "  "));
		}
	}

	private void fieldsVsInitializers(TableFields fields, boolean initializersWanted) throws FisherException {
		for (IdWithOptInit ii : fields.idInits) {
			boolean hasInit = ii.init != null;
			if (hasInit != initializersWanted) {
				String sherr = initializersWanted ? TABLE_QUERY_FIELDS_NEED_INITIALIZERS
						: TABLE_FIELDS_CANNOT_HAVE_INITIALIZERS;
				fields.flag(ERROR, sherr, "", ii);
			}
		}
	}

	@Override
	public void visit(MethodCall syn, IngestionContext arg) throws FisherException {
		syn.sig = new MethodSig(syn.method.str(), syn.args.size());
	}

	@Override
	public void visit(SuperCall syn, IngestionContext arg) throws FisherException {
		syn.sig = new MethodSig(syn.method.str(), syn.args.size());
	}

	@Override
	public void visit(QueryTable syn, IngestionContext arg) throws FisherException {
		checkInitsAndMaps(syn, true, syn.items);
	}

	@Override
	public void visit(QueryGroup syn, IngestionContext arg) throws FisherException {
		// Ignore for now.

	}

	@Override
	public void visit(Literal syn, IngestionContext arg) throws FisherException {
		syn.thing = EvalUtil.thingify(syn.value, syn);
	}

	@Override
	public void visit(PatLiteral syn, IngestionContext arg) throws FisherException {
		syn.thing = EvalUtil.thingify(syn.value, syn);
	}

	@Override
	public void visit(Match syn, IngestionContext arg) throws FisherException {
		List<Case> cases = syn.cases;
		int prevPrio = Integer.MAX_VALUE;
		for (Case c : cases) {
			if (c.from != null) {
				c.flag(ERROR, FROM_IN_MATCH, "");
			}
			if (prevPrio < c.prio) {
				c.flag(ERROR, MATCH_PRIO_IN_ORDER, "");
			}
			prevPrio = c.prio;
		}

	}

	@Override
	public void visit(Spawn syn, IngestionContext arg) throws FisherException {
		for (ProcMember pm : syn.bits) {
			if (pm instanceof LocalMember) {
				LocalMember lm = (LocalMember) pm;
				syn.localMembers.add(lm);
			} else if (pm instanceof FunDecl) {
				FunDecl fd = (FunDecl) pm;
				syn.funs.add(fd);
			} else if (pm instanceof HighLevelCommunication) {
				HighLevelCommunication hl = (HighLevelCommunication) pm;
				syn.highLevelCommunications.add(hl);
			} else if (pm instanceof ProcInit) {
				// In some odd cases (spawn inside of before/after, for one)
				// it is possible for the same spawn to get ingested repeatedly.
				// The syn.init==pm check here renders that harmless.
				if (syn.init == null || syn.init == pm) {
					syn.init = (ProcInit) pm;
				} else {
					pm.flag(ERROR, A_SPAWN_CAN_HAVE_ONLY_ONE_INIT, "");
				}
			} else if (pm instanceof ProcBody) {
				// See note in ProcInit case about multiple ingestion.
				if (syn.body == null || syn.body == pm) {
					syn.body = (ProcBody) pm;
				} else {
					pm.flag(ERROR, A_SPAWN_CAN_HAVE_ONLY_ONE_BODY, "");
				}
			} else {
				pm.flag(INTERNAL_ERROR, "Unknown spawn member", "");
			}
		}
		if (syn.body == null) {
			syn.flag(ERROR, A_SPAWN_MUST_HAVE_A_BODY_CLAUSE, "");
		}
		DistDesugarer.addSpawnuments(syn);
	}

	@Override
	public void visit(Recv syn, IngestionContext arg) throws FisherException {
		List<List<Case>> casesByPrio = new ArrayList<List<Case>>();
		syn.casesByPrio = casesByPrio;
		Map<Integer, List<Case>> caseMap = new HashMap<Integer, List<Case>>();
		for (Case c : syn.cases) {
			int p = c.prio;
			if (caseMap.containsKey(p)) {
				caseMap.get(p).add(c);
			} else {
				caseMap.put(p, Bard.list(c));
			}
		}
		List<Integer> priorities = new ArrayList<Integer>(caseMap.keySet());
		Collections.sort(priorities);
		for (Integer prio : priorities) {
			casesByPrio.add(0, caseMap.get(prio));
		}
		/* Or, in Thorn: 
		 * caseMap = %group(prio=prio){casesAtPrio = %list c; | for c && {: prio :} <- syn.cases};
		 * syn.casesByPrio := %sort(casesAtPrio %< prio | for {: prio, casesAtPrio :} <- caseMap);
		 */
	}

	@Override
	public void visit(Serve syn, IngestionContext arg) throws FisherException {
		ComponentInfo ci = surroundingComponentInfo(syn);
		if (ci != null) {
			syn.componentInfo = ci;
		} else {
			syn.flag(DangerLevel.ERROR, "'serve' must appear inside a spawn", "");
		}
		Cmd serve = DistDesugarer.serve(syn);
		syn.actualCode = serve;

	}

	private static ComponentInfo surroundingComponentInfo(Serve syn) {
		Syntax cursor = syn.parent();
		while (cursor != null && !(cursor instanceof ComponentInfo)) {
			cursor = cursor.parent();
		}
		return cursor == null ? null : (ComponentInfo) cursor;
	}

	@Override
	public void visit(JavalyFun syn, IngestionContext arg) throws FisherException {
		QualName packageAndClassQN = syn.impl.butlast();
		String packageAndClass = packageAndClassQN.toString();
		int nArgs = syn.formals.size();
		String methodName = syn.impl.last().str();
		try {
			Class cls = Class.forName(packageAndClass);
			syn.method = seekThingMethod(cls, methodName, nArgs, syn);
		} catch (SecurityException e) {
			syn.flag(DangerLevel.ERROR, "Security exception for " + syn, e.toString());
		} catch (ClassNotFoundException e) {
			syn.flag(DangerLevel.ERROR, "Java class not found in importing: class=" + packageAndClass, e.toString());
		}
	}

	public static Method seekThingMethod(Class cls, String methodName, int nArgs, Syntax src) {
		Class[] argCls = new Class[nArgs];
		for (int i = 0; i < nArgs; i++)
			argCls[i] = Thing.class;
		try {
			Method method = cls.getMethod(methodName, argCls);
			return method;
		} catch (Exception e) {
			src.flag(DangerLevel.ERROR, "No suitable method named " + methodName + " with " + nArgs
					+ " Thing arguments in " + cls, e.toString());
			return null;
		}
	}

	public static Constructor seekThingCtor(Class cls, int nArgs, Syntax src) {
		Class[] argCls = new Class[nArgs];
		for (int i = 0; i < nArgs; i++)
			argCls[i] = Thing.class;
		try {
			Constructor ctor = cls.getConstructor(argCls);
			return ctor;
		} catch (Exception e) {
			src.flag(DangerLevel.ERROR, "No suitable constructor " + " with " + nArgs + " Thing arguments in " + cls, e
					.toString());
			return null;
		}
	}

	public static Field seekField(Class cls, String name, Syntax src) {
		try {
			Field f = cls.getField(name);
			return f;
		} catch (SecurityException e) {
			src.flag(DangerLevel.ERROR, "Field " + name + " is not visible, or something.", "", src);
			return null;
		} catch (NoSuchFieldException e) {
			src.flag(DangerLevel.ERROR, "Field " + name + " does not exist, or something.", "", src);
			return null;
		}

	}

	public void visit(JavalyClassDecl syn, IngestionContext arg) throws FisherException {
		String packageAndClass = syn.impl.toString(); // Unlike functions, we don't need to peel off a final function name.
		try {
			Class cls = Class.forName(packageAndClass);
			syn.cls = cls;
			// Every class defines str(), ==, and hashCode 
			ThingExtended.addMethod(cls, "str", 0, seekThingMethod(cls, "str", 0, null), null, true);
			ThingExtended.addMethod(cls, "==", 1, seekThingMethod(cls, "eq", 1, null), null, true);
			ThingExtended.addMethod(cls, "hashCode", 0, seekThingMethod(cls, "th_hashcode", 0, null), null, true);

			// Now, the methods actually specified there
			for (JavalyMethodDecl jam : syn.methods) {
				int arity = jam.formals.size();
				String javaMethodName = jam.impl.str();
				Method method = seekThingMethod(cls, javaMethodName, arity, jam);
				ThingExtended.addMethod(cls, jam.name.str(), arity, method, jam, false);
			}
			for (JavalyNewDecl jam : syn.ctors) {
				int arity = jam.formals.size();
				Constructor ctor = seekThingCtor(cls, arity, jam);
				ThingExtended.addConstructor(cls, arity, ctor, jam);
			}
			for (Id id : syn.fields) {
				String name = id.str();
				Field field = seekField(cls, name, id);
				ThingExtended.addField(cls, name, field, syn);
			}
		} catch (ClassNotFoundException e) {
			syn.flag(DangerLevel.ERROR, "No class definition found for " + packageAndClass, "");
			e.printStackTrace();
		} catch (RuntimeException e) {
			syn.flag(DangerLevel.ERROR, "Something bad and perplexing happened: " + e, "");
			e.printStackTrace();
		}
	}

}
