
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.run;

import java.io.File;
import java.util.List;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.ingest.Ingester;
import fisher.parser.FisherParser;
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.runtime.ListTh;
import fisher.runtime.SiteTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.SiteData;
import fisher.statics.Env;
import fisher.statics.ModuleStatic;
import fisher.statics.PredefinedIdentifiers;
import fisher.statics.Seal;
import fisher.statics.SealForModule;
import fisher.statics.Sealant;
import fisher.syn.Cmd;
import fisher.syn.Module;
import fisher.syn.ModulesInAList;
import fisher.syn.ProcBody;
import fisher.syn.Seq;
import fisher.syn.Spawn;
import fisher.syn.SpawnByComponentName;
import fisher.syn.SyntaxInAList;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ProcMember;
import fisher.test.SealTest;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.CompilerMessage;
import fisher.util.DangerLevel;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import fisher.util.Security;

public  class  Thorn  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static String libPath() {
		String userdir = System.getenv("FISHER"); // path to 'fisher'.
		if (userdir == null)
			userdir = System.getProperty("user.dir");
		if (!userdir.endsWith("/"))
			userdir += "/";
		return userdir + "lib/";
	}

	public static List<ModuleStatic> builtInModules = Bard.list();

	static List<FisherSource> moduleFiles = Bard.list();
	static int port = 4260;
	static String host = "localhost";
	public static FisherSource spawnSrc = null;
	public static FisherSource evalSrc = null;
	public static FisherSource oneSrc = null;
	static String moduleCode;
	public static ListTh thornArgs = null;
	public static boolean printResultOfEval = false;
	public static boolean sandbox = false;

	public static boolean spawnOfEval = false;

	public static void main(String[] args) {
		try {
			parseArgs(args);
			if (oneSrc != null) {
				spawnOne();
			} else if (spawnOfEval)
				spawnOfEvalAndRun();
			else if (spawnSrc != null) {
				spawnAndRun();
			} else if (evalSrc != null) {
				evalAndRun();
			} else {
				die("I need source to run! '-1 filename.th to evaluate a file of modules and code blocks. -f foo.th' to evaluate a file; '-s oof.th' to spawn, '-sf ofo.th' to run file as body of spawn");
			}
		} catch (Exception e) {
			System.err.println("Exception: " + e);
			System.exit(1);
		}
	}

	private static void spawnOfEvalAndRun() {
		try {
			TestUtils.reset("--spawn of eval--");
			if (evalSrc == null)
				die("No Thorn file specified.");
			if (!evalSrc.exists())
				die("Thorn file " + evalSrc + " does not exist.");

			Cmd cmd = (Cmd) prepare(evalSrc, moduleFiles, SyntacticClass.STMTS, true);

			if (cmd == null) {
				die("Something bad happened.");
			}
			if (thereAreErrors()) {
				die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
			}

			SiteTh site = new SiteTh(host, port);
			Spawn spawn = (Spawn) cmd;
			SiteData sd = SiteData.start(site, spawn, Frame.rootFrame());

		} catch (FisherException e) {
			//			e.printStackTrace();
			System.err.println("Runtime Exception: " + e);
			System.exit(1);
		}
	}

	private static void spawnOne() {
		try {
			TestUtils.reset("--- Spawn One ---");
			if (oneSrc == null)
				die("No Thorn file specified.");
			if (!oneSrc.exists())
				die("Thorn file " + oneSrc + " does not exist.");

			Cmd cmd = (Cmd) prepare(oneSrc, moduleFiles, SyntacticClass.ONE_SRC, true);

			if (cmd == null) {
				die("Something bad happened.");
			}
			if (thereAreErrors()) {
				die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
			}

			if (cmd instanceof Spawn) {
				SiteTh site = new SiteTh(host, port);
				Spawn spawn = (Spawn) cmd;
				SiteData sd = SiteData.start(site, spawn, Frame.rootFrame());
			}
			else {
				Thing res = Evaller.fullEval(cmd);
				if (printResultOfEval) {
					System.out.println(EvalUtil.toString(res));
				}
			}
			

		} catch (Throwable thr) {
			System.err.println("Runtime Exception: " + thr);
			System.exit(1);
		}
	}

	private static void spawnAndRun() {
		try {
			TestUtils.reset("--spawn--");
			if (spawnSrc == null)
				die("No Thorn file specified.");
			if (!spawnSrc.exists())
				die("Thorn file " + spawnSrc + " does not exist.");

			Cmd cmd = (Cmd) prepare(spawnSrc, moduleFiles, SyntacticClass.SPAWN, false);

			if (cmd == null) {
				die("Something bad happened.");
			}
			if (thereAreErrors()) {
				die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
			}

			SiteTh site = new SiteTh(host, port); 
			Spawn spawn = (Spawn) cmd;
			SiteData sd = SiteData.start(site, spawn, Frame.rootFrame());

		} catch (FisherException e) {
			//			e.printStackTrace();
			System.err.println("Runtime Exception: " + e);
			System.exit(1);
		}
	}

	private static void evalAndRun() {
		try {
			TestUtils.reset("--run--");
			if (evalSrc == null)
				die("No Thorn file specified.");
			if (!evalSrc.exists())
				die("Thorn file " + evalSrc + " does not exist.");

			Cmd cmd = (Cmd) prepare(evalSrc, moduleFiles, SyntacticClass.STMTS, false);

			if (thereAreErrors()) {
				die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
			}
			Thing res = Evaller.fullEval(cmd);
			if (printResultOfEval) {
				System.out.println(EvalUtil.toString(res));
			}
		} catch (FisherException e) {
			System.err.println("Runtime Exception: " + e);
			System.exit(1);
			//			e.printStackTrace();
		}
	}

	public static boolean thereAreErrors() {
		for (CompilerMessage msg : Compilation.current.messages) {
			if (msg.danger == DangerLevel.ERROR) {
				return true;
			}
		}
		return false;
	}

	public static void prepare2(List<Module> modules, Syntax syn) throws FisherException {
		for (Module module : modules) {
			module.fillInParentPtrs();
			Ingester.check(module);
		}
		List<ModuleStatic> moduleStatics = ModuleStatic.bareStaticsFor(modules);
		syn.fillInParentPtrs();
		Ingester.check(syn);
		Sealant sealant = new Sealant();
		Env env = Env.root(moduleStatics);

		// Seal the modules...
		for (ModuleStatic moduleStatic : moduleStatics) {
			SealForModule container = moduleStatic.src.moduleSeal;
			Env inner = env.innerWithNewContainer(container);
			moduleStatic.src.accept(sealant, inner);
			Sealant.finalCheck(moduleStatic.src);

		}
		syn.accept(sealant, env);
		Sealant.finalCheck(syn);

		if (sandbox)
			Security.goToSandbox();
	}

	public static Syntax prepare(FisherSource sourceCode, List<FisherSource> moduleSources, SyntacticClass cls,
			boolean wrapInSpawnIfItSpawns) {
		try {
			Compilation.current.reset();
			List<Module> modules = Bard.list();
			for (FisherSource modSrc : moduleSources) {
				FisherParser parser = modSrc.parser();
				ModulesInAList mial = parser.TestModules(modSrc);
				modules.addAll(mial.modules);
			}

			// Code body proper: 
			FisherParser parser = sourceCode.parser();
			Syntax syn = cls.parse(parser, sourceCode);

			if (syn instanceof SyntaxInAList) {
				SyntaxInAList sial = (SyntaxInAList) syn;
				List<Cmd> cmds = Bard.list();
				for (Syntax s : sial.syns) {
					if (s instanceof Module) {
						Module ms = (Module) s;
						modules.add(ms);
					} else if (s instanceof Cmd) {
						Cmd cmd = (Cmd) s;
						cmds.add(cmd);
					} else {
						System.err.println("Internal error -- what is a " + s.getClass() + " doing here? \n" + s);
					}
				}
				if (cmds.size() == 1)
					syn = cmds.get(0);
				else
					syn = new Seq(syn.start, syn.end, cmds);
			}

			if (wrapInSpawnIfItSpawns && anyspawns(syn, modules)) {
				Id spawnName = new Id(syn.start, "©your program©", true);
				ProcMember spawnBody = new ProcBody(syn.start, syn.end, (Cmd) syn);
				syn = new Spawn(syn.start, syn.end, spawnName, Bard.list((ProcMember) spawnBody));
			}

			prepare2(modules, syn);

			return syn;

		} catch (Throwable e) {
			System.err.println("Parse Error: " + e);
			System.exit(1);
			return null;
		}
	}

	public static boolean anyspawns(Syntax syn, List<Module> modules) {
		if (anyspawns(syn))
			return true;
		for (Module module : modules) {
			if (anyspawns(module))
				return true;
		}
		return false;
	}

	public static boolean anyspawns(Syntax syn) {
		final List<Syntax> descendants = syn.computeDescendants();
		for (Syntax d : descendants) {
			if (d instanceof Spawn || d instanceof SpawnByComponentName) {
				return true;
			}
		}
		return false;
	}

	public static Syntax OLDprepare(FisherSource sourceCode, List<FisherSource> moduleSources, SyntacticClass cls,
			boolean wrapInSpawn) {
		try {
			Compilation.current.reset();
			List<ModuleStatic> moduleStatics = Bard.list();
			// This part: cf. SealTest.sealize()
			for (FisherSource modSrc : moduleSources) {
				FisherParser parser = modSrc.parser();
				ModulesInAList mial = parser.TestModules(modSrc);
				mial.fillInParentPtrs();
				Ingester.check(mial);
				List<ModuleStatic> mialStatics = ModuleStatic.bareStaticsFor(mial.modules);
				moduleStatics.addAll(mialStatics);
			}

			// Code body proper: 
			FisherParser parser = sourceCode.parser();
			Syntax syn = cls.parse(parser, sourceCode);
			if (wrapInSpawn) {
				Id spawnName = new Id(syn.start, "©your program©", true);
				ProcMember spawnBody = new ProcBody(syn.start, syn.end, (Cmd) syn);
				syn = new Spawn(syn.start, syn.end, spawnName, Bard.list((ProcMember) spawnBody));
			}
			syn.fillInParentPtrs();
			Ingester.check(syn);
			Sealant sealant = new Sealant();
			Env env = Env.root(moduleStatics);

			// Seal the modules...
			for (ModuleStatic moduleStatic : moduleStatics) {
				SealForModule container = moduleStatic.src.moduleSeal;
				Env inner = env.innerWithNewContainer(container);
				moduleStatic.src.accept(sealant, inner);
				Sealant.finalCheck(moduleStatic.src);

			}
			syn.accept(sealant, env);
			Sealant.finalCheck(syn);

			if (sandbox)
				Security.goToSandbox();

			return syn;

		} catch (ParseException e) {
			// TODO Auto-generated catch block
			System.err.println("Parse Exception: " + e);
			System.exit(1);
			//e.printStackTrace();
			return null;
		} catch (FisherException e) {
			// TODO Auto-generated catch block
			System.err.println("Fisher Exception: " + e);
			//			e.printStackTrace();
			System.exit(1);
			return null;
		}

	}

	private static void parseArgs(String[] args) {
		for (int i = 0; i < args.length; i++) {
			String s = args[i];
			if (s.equals("-m")) {
				snagModuleFiles(++i, args);
			} else if(s.equals("-h")){
				snagHost(++i, args);
			} 
			else if (s.equals("-s")) {
				snagThornFileSpawn(++i, args);
			} else if (s.equals("-f")) {
				snagThornFileEval(++i, args);
			} else if (s.equals("-sf")) {
				snagSpawnOfEval(++i, args);
			} else if (s.equals("--safer")) {
				sandbox = true;
			} else if (s.equals("-1")) {
				snagOneFileFitsAll(++i, args);
			} else if (s.equals("-p")) {
				snagPort(++i, args);
			} else if (s.equals("-np") || s.equals("--noprint")) {
				printResultOfEval = false;
			} else if (s.equals("--")) {
				List<StringTh> strings = Bard.list();
				for (int j = i + 1; j < args.length; j++)
					strings.add(StringTh.of(args[j]));
				thornArgs = ListTh.fromJavaList(strings);
				return;
			} else {
				die("-m moduleFileNamesSepBySemicolons -m moreOfThem [-s thornFileToSpawn.th | -f thornFileToRun.th] -p port -h host [-np | --noprint] -- thornArgs");
			}
		}
	}

	private static void die(String s) {
		System.err.println(s);
		System.exit(-1);
	}

	private static void snagOneFileFitsAll(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a filename after -1");
		}
		String onefilename = args[pos];

		File onefile = new File(onefilename);

		oneSrc = new FisherSource.FromFile(onefile);
	}

	private static void snagModuleFiles(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a semicolon-separated list of file names after -m");
		}
		String modsemi = args[pos];
		String[] mods = modsemi.split(";");
		for (String m : mods) {
			File f = new File(m);
			moduleFiles.add(new FisherSource.FromFile(f));
		}
	}

	private static void snagThornFileSpawn(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a file name after -s");
		}
		String thornfn = args[pos];
		spawnSrc = new FisherSource.FromFile(new File(thornfn));
	}

	private static void snagSpawnOfEval(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a file name after -f");
		}
		String thornfn = args[pos];
		evalSrc = new FisherSource.FromFile(new File(thornfn));
		spawnOfEval = true;
	}

	private static void snagThornFileEval(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a file name after -f");
		}
		String thornfn = args[pos];
		evalSrc = new FisherSource.FromFile(new File(thornfn));
	}

	private static void snagPort(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a port after -p");
		}
		String ports = args[pos];
		port = Integer.parseInt(ports);
	}
	
	private static void snagHost(int pos, String[] args){
		if(pos >= args.length)
			die("Please put a host after -h");
		host = args[pos];
	}

}
