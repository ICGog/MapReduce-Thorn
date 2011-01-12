
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

import static fisher.test.DistTest.distTest;

import java.io.File;
import java.io.FilenameFilter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.interfaces.Framelike;
import fisher.parser.SyntacticClass;
import fisher.runtime.RecordTh;
import fisher.runtime.SiteTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ComponentThread;
import fisher.runtime.dist.SiteData;
import fisher.syn.Cmd;
import fisher.syn.Probe;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ComponentInfo;
import fisher.util.Bard;
import fisher.util.FisherException;
import junit.framework.TestCase;

public  class  DistTest extends TestCase  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	private static final boolean whiny = !true;
	private static int[] SHORT_BACKOFF = new int[] { 10, 30, 100, 300, 1000, 3000, 1500, 12000, 12000 };
	private static int[] LONG_BACKOFF = new int[] { 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000 };

	// Long backoffs give the human a chance to stop things in the debugger
	private static int[] backoffs = true ? SHORT_BACKOFF : LONG_BACKOFF;

	private static StringBuffer whatIsNotDoneBuffer = new StringBuffer();

    private static int port = 4385;

	public static void distTest(String subdir) throws Exception {
		TestUtils.say(TestUtils.test, "starting distTest(" + subdir + ")");
		try {
			whatIsNotDoneBuffer = new StringBuffer();
			String loc = "dist/" + subdir;

			if (whiny)
				System.err.println("DistTest.distTest : start of a new distTest --- " + subdir);

			TestUtils.resetEverythingButLatches(loc);
			String fullDirName = TestUtils.testFileRoot() + "/dist/" + subdir;
			File fullDir = new File(fullDirName);
			String modules = Bard.contentsOf(fullDirName + "/Modules.thm");
			String[] thornFiles = fullDir.list(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.endsWith(".th");
				}
			});

			List<SiteData> sds = Bard.list();
			Cmd someCode = null;

			TestUtils.testStartLatch = new CountDownLatch(1); // the 1 is the test process.

			List<Object> ports = Bard.list();
			List<String> fns = Bard.list();

			for (String fn : thornFiles) {
				String secretaryCode = Bard.contentsOf(fullDirName + "/" + fn);
				if (whiny)
					System.err.println("distTest: starting " + fn + " on port " + port);
				String base = fn.substring(0, fn.indexOf(".th"));
				// fooPort: 4385
				fns.add(base + "Port");
				ports.add(port);
				SiteData sd = evaldist(subdir, secretaryCode, SyntacticClass.STMT, modules, port++);
				// fooSite : its site.
				fns.add(base + "Site");
				ports.add(sd.siteTh);
				// fooComp : its secretary componentTh
				fns.add(base + "Comp");
				ports.add(sd.secretary.compTh);
				someCode = (Cmd) sd.secretary.component;
				sds.add(sd);
			}

			TestUtils.recOfPorts = RecordTh.make(null, fns, ports);

			TestUtils.testStartLatch.countDown();

			boolean seenCheckPhase = false;

			// Now, try to wait 'til all the sites are done.

			boolean allDone = false;
			for (int backoff : backoffs) {
				if (allAreFinished(sds)) {
					allDone = true;
					break;
				}
				Thread.sleep(backoff);
			}

			if (whiny) {
				System.err.println(allDone ? "~~~~~~ all done ~~~~~~" : " ~!~!~!~!~!~!~! not all done !~!~!~!~!!~!~ ");
			}

			// Look for exceptions first.
			List<Exception> exns = TestUtils.exceptionsSnapshot();
			if (!exns.isEmpty()) {
				StringWriter sw = new StringWriter();
				StringBuffer sb = new StringBuffer();
				sb.append(loc + "/exceptions(" + exns.size() + ")" + "\n\n");

				for (Exception exception : exns) {
					sb.append(exception.getMessage() + "\n~~~\n");
					sb.append(Bard.sep(exception.getStackTrace(), "\n"));
					sb.append("\n\n˝˝˝˝˝\n\n");

				}

				String msg = sb.toString();

				assertTrue(msg, false);
			}
			if (!allDone) {
				String unfinishedStrings = whatIsNotDoneBuffer.toString();
				assertTrue(loc + "/waity (unfinished = " + unfinishedStrings + ")", allDone);
			}

			// Look for ~!@s
			for (SiteData siteData : sds) {
				EvalTest.checkProbes(loc + "/" + siteData.secretary.component.name(),
						(Cmd) siteData.secretary.component);
				if (EvalTest.hasCheckPhase((Cmd) siteData.secretary.component)) {
					seenCheckPhase = true;
				}
			}

			assertTrue(loc + "/has-check-phase", seenCheckPhase);

			// Clean up
			closingServerSockets: for (ServerSocket ss : TestUtils.serverSockets) {
				// The ConnectionAcceptingThreads are trying to close the server sockets.  Give them a chance...
				for (int b : backoffs) {
					if (ss.isClosed())
						continue closingServerSockets;
					Thread.sleep(b);
				}
				assertTrue(loc + "/close/ Server socket " + ss + " didn't close in a timely fashion.", false);
			}
			TestUtils.serverSockets.clear();
			TestUtils.reset(loc); // clear latches too.
		} finally {
			TestUtils.say(TestUtils.test, "ending distTest(" + subdir + ")");
		}
	}

	private static boolean allAreFinished(List<SiteData> sds) {
		whatIsNotDoneBuffer = new StringBuffer();
		for (SiteData siteData : sds) {
			whatIsNotDoneBuffer.append("[about " + siteData + "]");
			if (!siteData.allDoneAltogether()) {
				whatIsNotDoneBuffer.append(" " + siteData + "(" + siteData.unfinishedThreadNames() + ")");
				return false;
			}
		}
		return true;
	}

	public static SiteData evaldist(String loc, String code, SyntacticClass cls, String moduleCode, int port)
			throws Exception {

		final Syntax prespawn = EvalTest.prep(loc + "/prep", code, cls, moduleCode);
		if (!(prespawn instanceof ComponentInfo)) {
			throw new Exception(loc + "/cast/ It\'s not a component or spawn:" + (prespawn.getClass()) + "\n"
					+ prespawn);
		}
		ComponentInfo spawn = (ComponentInfo) prespawn;
		try {
			SiteTh site = new SiteTh("localhost", port);
			SiteData sd = SiteData.start(site, spawn, Frame.rootFrame());
			return sd;
		} catch (FisherException fe) {
			throw new Exception(loc + "/eval", fe);
		} catch (RuntimeException re) {
			throw new Exception(loc + "/eval", re);
		}

	}

	public void test001() throws Exception {
		distTest("001");
	}

	public void test002() throws Exception {
		distTest("002");
	}

	public void test003() throws Exception {
		distTest("003");
	}

	public void test004() throws Exception {
		distTest("004");
	}

	public void test005() throws Exception {
		distTest("005-bif");
	}

	public void test006() throws Exception {
		distTest("006");
	}

	public void test007() throws Exception {
		distTest("007");
	}

	public void test008() throws Exception {
		distTest("008");
	}

	public void test009() throws Exception {
		distTest("009");
	}

	public void test010() throws Exception {
		distTest("010");
	}

	public void test011() throws Exception {
		distTest("011-mod");
	}

	public void test012() throws Exception {
		distTest("012-llhlex");
	}

	public void test013() throws Exception {
		distTest("013-halvsies");
	}

	public void test014() throws Exception {
		distTest("014-hl");
	}

	public void test015() throws Exception {
		distTest("015");
	}

	public void test016() throws Exception {
		distTest("016");
	}

	public void test017() throws Exception {
		distTest("017");
	}

	public void test018() throws Exception {
		distTest("018");
	}

	public void test019() throws Exception {
		distTest("019");
	}

	public void test020() throws Exception {
		distTest("020-sync-in-sync");
	}

	public void test021() throws Exception {
		distTest("021-worker");
	}

	public void test022() throws Exception {
		distTest("022");
	}

	public void test023() throws Exception {
		distTest("023");
	}

	public void test025() throws Exception {
		distTest("025-rev-llhlex");
	}

	public void test024() throws Exception {
		distTest("024-envelope");
	}

	public void test026() throws Exception {
		distTest("026-timeout");
	}

	public void test027() throws Exception {
		distTest("027-timeout");
	}

	public void test028() throws Exception {
		distTest("028-obj");
	}

	public void test029() throws Exception {
		distTest("029");
	}

	public void test030() throws Exception {
		distTest("030-ss");
	}

	public void test031() throws Exception {
		distTest("031-http");
	}

	public void test032() throws Exception {
		distTest("032-componentdecl");
	}

	public void test033() throws Exception {
		distTest("033-oopsla");
	}

	public void test034() throws Exception {
		distTest("034-servecatch");
	}

	public void test035() throws Exception {
		distTest("035-oopsla-daterbase");
	}

	public void test036() throws Exception {
		distTest("036-oopsla-worker");
	}

	public void test037() throws Exception {
		distTest("037-null");
	}

	public void test038() throws Exception {
		distTest("038-oopsla");
	}

	public void test039() throws Exception {
		distTest("039-dining");
	}

	public void test040() throws Exception {
		distTest("040-semaphore");
	}

	public void test041() throws Exception {
		distTest("041-param-comp");
	}
	public void test042() throws Exception {
		distTest("042-import");
	}
	public void test043() throws Exception {
		distTest("043-crash");
	}
	public void test044() throws Exception {
		distTest("044-oopsla-auth");
	}
	public void test045() throws Exception {
		distTest("045-jakob");
	}
	public void test046() throws Exception {
		distTest("046-site2");
	}
	public void test047() throws Exception {
		distTest("047-http-2");
	}
	public void test048() throws Exception {
		distTest("048-security-1");
	}
	
}
