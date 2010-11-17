
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

import static fisher.parser.SyntacticClass.MODUS;

import java.io.BufferedInputStream;
import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import java.util.NoSuchElementException;

import javax.print.attribute.standard.PrinterInfo;

import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.ingest.Ingester;
import fisher.parser.FisherParser;
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.statics.Env;
import fisher.statics.ModuleStatic;
import fisher.statics.Sealant;
import fisher.syn.Cmd;
import fisher.syn.Module;
import fisher.syn.ModulesInAList;
import fisher.syn.core.Syntax;
import fisher.test.SealTest;
import fisher.test.TestUtils;
import fisher.util.Bard;
import fisher.util.Compilation;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import fisher.util.Security;

public  class  REPL  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public static Scanner inscanner = new Scanner(System.in);
	public static Env env = Env.root(Collections.EMPTY_LIST);
	public static Sealant sealant = new Sealant();
	public static Frame frame = null;
	public static Evaller evaller = new Evaller(null, frame);
	public static List<ModuleStatic> loadedModules = Bard.list();
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		peerAtArgs(args);

		try {
			frame = Frame.rootFrame();
		} catch (FisherException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		System.out.println("Welcome to Thorn's REPL!  #? for help.");
		
		repl: while (true) {
			try {
				System.out.println("Thorn: ");
				Compilation.current.reset();
				String code = inscanner.nextLine();
				if (code.startsWith("##")) {
					doLoad(code);
				}
				else if (code.startsWith("#?")) {
					doHelp();
				}
				else if (code.startsWith("#m")) {
					getModuleDefs(code);
				}
				else {
					Syntax syn = doEval(code);
				}
			} catch (NoSuchElementException e) {
			    // Happens on e.g., ^D.
			    System.err.println("Bye!\n");
			    return;
			} catch (Throwable e) {
			    System.err.println("Error: " + e);				
			} 

		}
	}
	
	private static void peerAtArgs(String[] args) {
		for(int i = 0; i < args.length;i++) {
			String arg = args[i];
			if (arg.equals("--safer")) Security.goToSandbox();
			else {
				System.err.println("Unknown argument: " + arg);
				throw new RuntimeException("I must now die because someone told me to " + arg);
			}
		}
	}
	
	private static void getModuleDefs(String code) throws FisherException{
		try {
			String fn = code.substring(2).trim();
			File f = new File(fn);
			FisherSource fs = new FisherSource.FromFile(f);
			FisherParser parser = fs.parser();
			ModulesInAList mial = parser.TestModules(fs);
			mial.fillInParentPtrs();
			Ingester.check(mial);
			List<ModuleStatic> mialStatics = ModuleStatic.bareStaticsFor(mial.modules);
			for (ModuleStatic moduleStatic : mialStatics) {
				env.addModule(moduleStatic);
			}
			Doom.notYet();
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (RuntimeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	private static void doHelp() {
		System.out.println("'## file' -- load a file");
		System.out.println("'#?' -- this help message");
		System.out.println("'#m file' -- take module definitions from file");
		System.out.println("Anything else is treated as Thorn code.");
	}

	private static void doLoad(String code) throws ParseException, FisherException {
		// Input is: ## filename
		String fn = code.substring(2).trim();
		File f = new File(fn);
		String c = Bard.contentsOf(f);
		doEval(c);
	}
	
	private static Syntax doEval(String code) throws ParseException, FisherException {
		Syntax syn = TestUtils.parse("/parse/", code, SyntacticClass.STMTLIST);
		Ingester.check(syn);
		syn.accept(sealant, env);
		Sealant.finalCheck(syn);
		if (SealTest.thereAreErrors()) {
			System.out.println("\n" + "\nThere were errors!\n "
					+ Bard.sep(Compilation.current.messages, "\n"));
		} else {
			fisher.runtime.Thing v = evaller.eval((Cmd) syn, frame);
			System.out.println("==> " + v);
		}
		return syn;
	}

}
