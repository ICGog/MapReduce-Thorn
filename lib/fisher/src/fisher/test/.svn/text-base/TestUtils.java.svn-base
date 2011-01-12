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

import java.io.File;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;

import junit.framework.Assert;

import fisher.run.FreakSyntax;
import fisher.runtime.ModuleDynamicTh;
import fisher.runtime.RecordTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.SiteData;
import fisher.syn.Probe;
import fisher.syn.chart.SynPtr;
import fisher.syn.core.*;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import fisher.util.Security;
import fisher.desugar.DesugarUtils;
import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.parser.*;
import static fisher.parser.SyntacticClass.*;

public class TestUtils {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	public static Object testScribble = null;

	private TestUtils() {
		throw new RuntimeException("Just a module d0d0");
	}

	public static boolean test = !true;
	public static boolean connections = !true;
	public static boolean printComponentThread = !true;
	public static boolean printMailbox = !true;
	public static boolean printSend = !true;
	public static boolean printRecv = !true;
	public static boolean printConnAccThr = !true;
	public static boolean printNewComponent = !true;
	public static boolean printAcquireSendStreamTo = !true;
	public static boolean asyncStmt = !!!true;
	public static boolean syncStmt = !true;
	public static boolean recv = !!!true;
	public static boolean desugarServe = !true;
	public static boolean http = !true;
	public static boolean stringSocketRecv = !true;

	private static int count = 0;
	public static final boolean COUNT = !!true;
	public static final boolean PRINT_FILES = !!!true;

	public static List<Exception> exceptions = new ArrayList<Exception>();

	public static void printCount() throws Exception {
		if (!COUNT)
			return;
		System.out.println("Count = " + count);
	}

	public static void count() {
		count++;
	}

	public static String loc = "(no loc)";

	public final static boolean TEST_ModuleInstanceTh = true;

	public static void resetEverythingButLatches(String loc) {
		hitCounts.clear();
		moduleInstances.clear();
		TestUtils.loc = loc;
		DesugarUtils.NumberGensyms = true;
		Evaller.reset();
		phaseBag.clear();
		SiteData.reset();
		exceptions.clear();
		serverSockets.clear();
		ThingExtended.reset();
		Security.reset(12321);
	}

	public static void resetLatches() {
		latches.clear();
	}

	public static void reset(String loc) {
		resetEverythingButLatches(loc);
		resetLatches();
	}

	public static void say(boolean b, String cause, Object... stuff) {
		if (b) {
			System.err.println(cause + " ~> " + Bard.sep(stuff, ""));
		}
	}

	public static void say(boolean b, Object cause, Object... stuff) {
		if (b) {
			String cloo = Bard.shortClassName(cause);
			if (cause instanceof ComponentThread) {
				ComponentThread ct = (ComponentThread) cause;
				cloo = ct.getName();
			}
			System.err.println(cloo + " ~> " + Bard.sep(stuff, ""));
		}
	}

	
	public static String testFileRoot() {
		if (! FreakSyntax.convert ) {
			String userdir = System.getProperty("user.dir");
			return userdir + "/testcase/";
		} else {
			System.err.println("Abnormal test director!");
			return "/Users/bard/thorn/tmp/nova/";
		}
		//		return "/Users/bard/thorn/fisherws/fisher/testcase/";
	}

	public static Syntax parse(String loc, String s, SyntacticClass cls) throws ParseException {
		count();
		return ParserUtils.parse(loc, s, cls);
	}

	public static String explain(String loc, String subject, String action) {
		return loc + "/" + action + "\n" + subject + "\n";
	}

	public static File testfile(String partialFileName) {
		return new File(testFileRoot() + partialFileName);
	}

	public static Syntax getDescendantByTypeAndIndex(Syntax syn, Class classToSeek, int whichOne) {
		int i = 0;
		for (Syntax desc : syn.computeDescendants()) {
			if (desc.getClass() == classToSeek) {
				i += 1;
				if (i == whichOne)
					return desc;
			}
		}
		return null;
	}

	public static String strParents(List<? extends Syntax> stuff, String sep) {
		List<String> L = new ArrayList<String>(stuff.size());
		for (Syntax syn : stuff) {
			Syntax p = syn.parent() == null ? syn : syn.parent();
			String p1 = p.toString();
			Syntax g = p.parent() == null ? p : p.parent();
			L.add(p1 + " €€€€€€€€€ " + g.toString());
		}
		return Bard.sep(L, sep);
	}

	public static void checkNoErrors(String loc, String caller, String code) throws Exception {
		if (Compilation.current.messages.isEmpty())
			return;
		String msg = loc + "/" + caller + " for " + code;
		Assert.assertTrue(msg, false);
	}

	public static void checkParentPointers(String loc, Syntax syn) throws Exception {
		List<Syntax> desc = syn.computeDescendants();
		for (Syntax d : desc) {
			List<Syntax> children = d.children();
			List<SynPtr> ptrs = d.synPtrs();
			int nptr = ptrs.size();
			Assert.assertEquals(loc + "/synPtr/size ", children.size(), nptr);
			for (int i = 0; i < nptr; i++) {
				SynPtr pi = ptrs.get(i);
				Syntax child = children.get(i);
				Assert.assertEquals(loc + "/synPtr/" + pi, child, pi.fetch());
			}
			// Make sure geneology is right...
			if (d == syn) {
				Assert.assertNull(loc + "/geneology-0", d.geneology);
				Assert.assertNull(loc + "geneology-00", d.parent());
			} else {
				Assert.assertEquals(loc + "/geneology", d, d.geneology.fetch());
				Syntax parent = d.parent();
				Assert.assertEquals(loc + "/geneology-a", d, parent.getChild(d.geneology.field, d.geneology.index));
			}
		}
	}

	public static List<Probe> probesIn(Syntax syn) {
		List<Probe> probes = new ArrayList<Probe>();
		for (Syntax chidl : syn.computeDescendants()) {
			if (chidl instanceof Probe) {
				Probe probe = (Probe) chidl;
				probes.add(probe);
			}
		}
		return probes;
	}

	///////////////////////////////////////////////////////////////////////////////////
	// dist -- server sockets (which will need closing)
	///////////////////////////////////////////////////////////////////////////////////

	public static final Set<ServerSocket> serverSockets = new HashSet<ServerSocket>();

	///////////////////////////////////////////////////////////////////////////////////
	// dist -- phase bag.
	///////////////////////////////////////////////////////////////////////////////////

	/*
	 * A phase bag is used to test distributed stuff. 
	 * It is a map from Thing -> Bag<Thing>  
	 *    (And both the map and the bags are synchronized).
	 * Usage: 
	 *   * As the distributed test case runs (in one VM, but maybe multi-site)
	 *     it will scribble into the phase bag.
	 *   * At the end of the test, the phase bag is compared 
	 *     to the desired state.  
	 */

	public static Map<Thing, List<Thing>> phaseBag = Collections.synchronizedMap(new HashMap<Thing, List<Thing>>());

	public synchronized static void phase(Thing phase, Thing note) {
		if (phaseBag.containsKey(phase)) {
			phaseBag.get(phase).add(note);
		} else {
			List phaseList = Collections.synchronizedList(Bard.list(note));
			phaseBag.put(phase, phaseList);
		}
	}

	public static synchronized Set<Thing> phases() {
		return phaseBag.keySet();
	}

	public static synchronized List<Thing> phased(Thing phase) {
		if (phaseBag.containsKey(phase))
			return phaseBag.get(phase);
		else
			return null;
	}

	///////////////////////////////////////////////////////////////////////////////////
	// Exception record
	///////////////////////////////////////////////////////////////////////////////////

	public static synchronized void noteException(Exception exn) {
		exceptions.add(exn);
	}

	public static synchronized List<Exception> exceptionsSnapshot() {
		return new ArrayList<Exception>(exceptions);
	}

	///////////////////////////////////////////////////////////////////////////////////
	// Distribution tools
	///////////////////////////////////////////////////////////////////////////////////

	public static CountDownLatch testStartLatch;

	public static RecordTh recOfPorts;

	public static Map<Thing, CountDownLatch> latches = new HashMap<Thing, CountDownLatch>();

	public static void latchmake(Object key, int count) throws FisherException {
		CountDownLatch latch = new CountDownLatch(count);
		latches.put(EvalUtil.thingify(key, null), latch);
	}

	///////////////////////////////////////////////////////////////////////////////////
	// hit count 
	///////////////////////////////////////////////////////////////////////////////////

	public static Map<Probe, Integer> hitCounts = Collections.synchronizedMap(new HashMap<Probe, Integer>());

	public synchronized static void hit(Probe probe) {
		int i;
		if (hitCounts.containsKey(probe)) {
			i = hitCounts.get(probe);
		} else {
			i = 0;
		}
		hitCounts.put(probe, i + 1);
	}

	public static int hitCount(Probe probe) {
		return hitCounts.containsKey(probe) ? hitCounts.get(probe) : 0;
	}

	///////////////////////////////////////////////////////////////////////////////////
	// module instances
	///////////////////////////////////////////////////////////////////////////////////
	public static List<ModuleDynamicTh> moduleInstances = new ArrayList<ModuleDynamicTh>();

	public static void noteModuleInstance(ModuleDynamicTh inst) {
		moduleInstances.add(inst);
	}

	public static boolean NO_CLASS_DESUGARING_SO_I_CAN_DO_PARSER_TESTS = false;

}
