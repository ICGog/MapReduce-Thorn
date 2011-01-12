
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
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.runtime.ListTh;
import fisher.runtime.SiteTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.SiteData;
import fisher.statics.Seal;
import fisher.statics.Sealant;
import fisher.syn.Cmd;
import fisher.syn.Spawn;
import fisher.syn.core.Id;
import fisher.syn.core.Syntax;
import fisher.syn.interfaces.ComponentInfo;
import fisher.test.SealTest;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.DangerLevel;
import fisher.util.FisherException;

public  class  OLD_Thorn  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	/**
	 * @param args
	 */

	static List<java.io.File> moduleFiles = Bard.list();
	static int port = 4260;
	static File spawnFile = null;
	static File evalFile = null;
	static String moduleCode;
	public static ListTh thornArgs = null;
	public static boolean printResultOfEval = true;

	public static void main(String[] args) {
		snagUniversals();
		parseArgs(args);
		if (spawnFile != null) {
			spawnAndRun();
		}
		else if (evalFile != null) {
			evalAndRun();
		}
		else {
			die("I need source to run! '-f foo.th' to evaluate a file; '-s oof.th' to spawn.");
		}
	}
	
	public static void snagUniversals(){
		
	}
	
	private static void evalAndRun() {
		try {
			TestUtils.reset("--run--");
			slurpModuleCode();
			if (evalFile == null)
				die("No Thorn file specified.");
			if (!evalFile.exists())
				die("Thorn file " + evalFile + " does not exist.");		
			String srcCode = Bard.contentsOf(evalFile);
			Cmd cmd = (Cmd) SealTest.sealize("thorn", srcCode, SyntacticClass.STMT, moduleCode);
			if (SealTest.thereAreErrors()) {
				die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
			}
			Thing res = Evaller.fullEval(cmd);
			if (printResultOfEval) {
				System.out.println(EvalUtil.toString(res));
			}
		} catch (ParseException e) {
			e.printStackTrace();
		} catch (FisherException e) {
			e.printStackTrace();
		}
	}
	
	private static Syntax prepare(String srcCode, String moduleCode) {
		Syntax res = null;
		try {
			res = SealTest.sealize("thorn", srcCode, SyntacticClass.STMT, moduleCode);
			//checkSealsOnAllIds(res);
			
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FisherException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (SealTest.thereAreErrors()) {
			die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
		}
		return res;
	}
	
	private static void  checkSealsOnAllIds( Syntax syn){
		List<Id> ids = Sealant.allIdsIn(syn);
		for (Id id : ids) {
			Seal seal = id.seal();
			if (seal == null) {
				id.flag(DangerLevel.ERROR, "Undefined identifier: " + id, "");
			}
		}
	}
	

	private static void spawnAndRun() {
		TestUtils.reset("--run--");
		slurpModuleCode();
		if (spawnFile == null)
			die("No Thorn file specified.");
		if (!spawnFile.exists())
			die("Thorn file " + spawnFile + " does not exist.");
		String srcCode = Bard.contentsOf(spawnFile);
		SiteTh site = new SiteTh("localhost", port);
		try {
			Syntax syn = prepare(srcCode, moduleCode); // SealTest.sealize("thorn", srcCode, SyntacticClass.STMT, moduleCode);

			if (SealTest.thereAreErrors()) {
				die("\n" + "\nThere were errors!\n " + Bard.sep(Compilation.current.messages, "\n"));
			}

			if (!(syn instanceof Spawn)) {
				die("Currently, the thorn code after -s must be a spawn.");
			}

			Spawn spawn = (Spawn) syn;
			SiteData sd = SiteData.start(site, spawn, Frame.rootFrame());
		} catch (FisherException e) {
			e.printStackTrace();
		} 
	}

	private static void slurpModuleCode() {
		StringBuffer moduleCodeSB = new StringBuffer();
		for (File mod : moduleFiles) {
			if (!mod.exists()) {
				die("Module file " + mod + " does not exist.");
			}
			moduleCodeSB.append(Bard.contentsOf(mod));
			moduleCodeSB.append("\n\n");
		}
		moduleCode = moduleCodeSB.toString();
	}

	private static void parseArgs(String[] args) {
		for (int i = 0; i < args.length; i++) {
			String s = args[i];
			if (s.equals("-m")) {
				snagModuleFiles(++i, args);
			} else if (s.equals("-s")) {
				snagThornFileSpawn(++i, args);
			} else if (s.equals("-f")) {
				snagThornFileEval(++i, args);
			} else if (s.equals("-p")) {
				snagPort(++i, args);
			} else if (s.equals("-np") || s.equals("--noprint")) {
				printResultOfEval = false;
			}
			else if (s.equals("--")) {
				List<StringTh> strings = Bard.list();
				for(int j = i+1; j < args.length; j++) strings.add(StringTh.of(args[j]));
				thornArgs = ListTh.fromJavaList(strings);
				return;
			}
			else {
				die("-m moduleFileNamesSepBySemicolons -m moreOfThem [-s thornFileToSpawn.th | -f thornFileToRun.th] -p port [-np | --noprint] -- thornArgs");
			}
		}
	}

	private static void die(String s) {
		System.err.println(s);
		System.exit(-1);
	}

	private static void snagModuleFiles(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a semicolon-separated list of file names after -m");
		}
		String modsemi = args[pos];
		String[] mods = modsemi.split(";");
		for (String m : mods) {
			File f = new File(m);
			moduleFiles.add(f);
		}
	}
	
	private static void snagThornFileSpawn(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a file name after -s");
		}
		String thornfn = args[pos];
		spawnFile = new File(thornfn);
	}

	private static void snagThornFileEval(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a file name after -f");
		}
		String thornfn = args[pos];
		evalFile = new File(thornfn);
	}

	private static void snagPort(int pos, String[] args) {
		if (pos >= args.length) {
			die("Please put a port after -p");
		}
		String ports = args[pos];
		port = Integer.parseInt(ports);
	}

}
