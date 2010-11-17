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
import java.net.ServerSocket;
import java.util.List;

import fisher.desugar.ComponentDeclDesugarer;
import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.Frame;
import fisher.eval.Matchiste;
import fisher.ingest.Ingester;
import fisher.parser.FisherParser;
import fisher.parser.FisherParser2;
import fisher.parser.ParseException;
import fisher.parser.SyntacticClass;
import fisher.runtime.ComponentTh;
import fisher.runtime.IntTh;
import fisher.runtime.ListTh;
import fisher.runtime.SiteTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.ConnectionAcceptingThread;
import fisher.runtime.dist.ConnectionThread;
import fisher.runtime.dist.SiteData;
import fisher.statics.Env;
import fisher.statics.ModuleStatic;
import fisher.statics.PredefinedIdentifiers;
import fisher.statics.Seal;
import fisher.statics.SealForModule;
import fisher.statics.Sealant;
import fisher.syn.Cmd;
import fisher.syn.ComponentDecl;
import fisher.syn.Formals;
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
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.FisherSource;
import fisher.util.Security;

public class RunXorn {
	static String copyright() {
		return fisher.util.Copyright.IBM_COPYRIGHT;
	}

	
	public static void main(String[] args) {
		try {
			spawnLocallyFromFile(IntTh.of(4334),
					new File("/Users/bard/thorn/tmp/bracing/b02.th"),
					Bard.list(new File("/Users/bard/thorn/tmp/bracing/Modules.thm")),
					null
					);
		} catch (FisherException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	/*
	 * ,
	I think I'm going to need to do that in two parts: 

	(1) 
	  spawnLocally(port, thornFile, thornModuleList, arguments)

	  retCompRefFlag can be: 
	    * null - return null (immediately)
	    * true - return the component ref (as soon as possible)
	    * false - return a nonce.  

	  port can be: 
	    * a number -- use that number.
	    * a range -- find an unused port in that range.

	 * 
	 */
	public static ComponentTh spawnLocallyFromFile(Thing port, File thornFile, List<File> thornModuleList, Thing arguments) throws FisherException {
		RunXorn runThorn = new RunXorn();
		return runThorn.spawnLocallyFromFile2(port, thornFile, thornModuleList, arguments);
	}

	public static Thing spawnLocallyFromFile(Thing port, Thing thornFileß, Thing thornModuleListß, Thing arguments)
			throws FisherException {
		File thornFile = EvalUtil.asJavaFile(thornFileß);
		List<File> thornModuleList = Bard.list();
		slamFilesInto(thornModuleListß, thornModuleList);
		return spawnLocallyFromFile(port, thornFile, thornModuleList, arguments);
	}
	
	
	private static void slamFilesInto(Thing ß, List<File> thornModuleList) throws FisherException {
		if (ß instanceof Iterable) {
			Iterable<Thing> itß = (Iterable) ß;
			for(Thing x : itß ) {
				slamFilesInto(x, thornModuleList);
			}
		}
		else {
			thornModuleList.add(EvalUtil.asJavaFile(ß));
		}
	}
	
	
	List<FisherSource> moduleSources = Bard.list();
	ServerSocket serverSocket = null;
	String host = "localhost";
	public FisherSource spawnSrc = null;
	public boolean sandbox = false;
	private SiteData spawnedSiteData;
	private List<Module> moduleSyns;
	private Spawn spawnSyn;
	private Formals formals;
	private ComponentDecl originalCompDecl =null;


	
	
	private ComponentTh spawnLocallyFromFile2(Thing port, File thornFile, List<File> thornModuleList, Thing arguments) throws FisherException {

		this.spawnSrc = new FisherSource.FromFile(thornFile);
		this.moduleSources = fisherSourceIze(thornModuleList);
		this.findServerSocket(port);
		this.parseEverything();
		this.sealEverything();
		this.spawnIt(arguments);
		ComponentTh spawnedCompTh = this.spawnedSiteData.secretary.compTh;

		return spawnedCompTh;

	}

	
	
	public void parseEverything() throws FisherException {
		Compilation.current.reset();
		
		// Parse the modules
		this.moduleSyns = Bard.list();
		for (FisherSource modSrc : moduleSources) {
			FisherParser2 parser = modSrc.parser2();
			try {
				ModulesInAList mial = parser.TestModules(modSrc);
				moduleSyns.addAll(mial.modules);
			} catch (ParseException e) {
				Doom.runtime("A module file (" + modSrc + ") didn't parse: \n" + e, null);
			}
		}
		
		// Parse the component to launch.
		FisherParser2 parser = spawnSrc.parser2();
		try {
			final Syntax parsed = SyntacticClass.SPAWN_OR_COMPONENT.parse2(parser, spawnSrc);
			if (parsed instanceof Spawn) {
				this.spawnSyn = (Spawn) parsed;
				this.formals = null;
			} 
			else if (parsed instanceof ComponentDecl) {
				ComponentDecl cd = (ComponentDecl) parsed;
				this.spawnSyn = ComponentDeclDesugarer.retrieveSpawn(cd);
				this.formals = ComponentDeclDesugarer.retrieveFormals(cd);
				this.originalCompDecl = cd;
			}
		} catch (ParseException e) {
			Doom.runtime("A source file (" + spawnSrc +") didn't parse:\n" + e, null);
		}
	}
	
	public void sealEverything() throws FisherException {
		for (Module module : moduleSyns) {
			module.fillInParentPtrs();
			Ingester.check(module);
		}
		List<ModuleStatic> moduleStatics = ModuleStatic.bareStaticsFor(moduleSyns);
		
		
		spawnSyn.fillInParentPtrs();
		Ingester.check(spawnSyn);
		
		Sealant sealant = new Sealant();
		Env env = Env.root(moduleStatics);

		// Seal the modules...
		for (ModuleStatic moduleStatic : moduleStatics) {
			SealForModule container = moduleStatic.src.moduleSeal;
			Env inner = env.innerWithNewContainer(container);
			moduleStatic.src.accept(sealant, inner);
			Sealant.finalCheck(moduleStatic.src);
		}
		Syntax synToSeal = originalCompDecl == null ? spawnSyn : originalCompDecl;
		synToSeal.accept(sealant, env);
		Sealant.finalCheck(synToSeal);

		if (sandbox)
			Security.goToSandbox();
		
	}
	
	public static List<FisherSource> fisherSourceIze(List<File> thornModuleList) {
		List<FisherSource> sources = Bard.list();
		for (File file : thornModuleList) {
			sources.add(new FisherSource.FromFile(file));
		}
		return sources;
	}

	public void findServerSocket(Thing port) throws FisherException {
		int lowPort, highPort;
		if (port.isLong()) {
			lowPort = (int) port.asLong(Evaller.lastSyntax());
			highPort = lowPort;
		} else if (port.isIntRange()) {
			lowPort = (int) port.asIntRange(null).min;
			highPort = (int) port.asIntRange(null).max;
		} else {
			Doom.runtime("The port given to findServerSocket must be an integer, or an int range.", Evaller
					.lastSyntax(), "Port=" + port, "Port type = " + EvalUtil.kind(port));
			return; 
		}

		//For convenience, we try a few ports at random ...  
		for(int i = 0; i < 5 && this.serverSocket == null; i++) tryToFindServerSocketOnPort( Bard.rand(lowPort, highPort));
		// Then try them all in order
		for(int i = lowPort; i <= highPort && this.serverSocket == null; i++) 
			tryToFindServerSocketOnPort(i);
		if (this.serverSocket == null) {
			Doom.runtime("No open server socket found (" + port + ")", Evaller.lastSyntax(), port);
		}
	}
	
	private static int[] backoffs = new int[]{10,30};
	
	private void tryToFindServerSocketOnPort (int port) throws FisherException {
		try {
			this.serverSocket = ConnectionAcceptingThread.acquireServerSocket(port, 0, backoffs);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void spawnIt(Thing arguments) throws FisherException {
		if (spawnSrc == null || thereAreErrors()) {
			Doom.runtime("There were errors!\n" +  Bard.sep(Compilation.current.messages, "\n"), null);
		}
		int port = serverSocket.getLocalPort();
		SiteTh site = new SiteTh(host, port);
		
		Frame f = Frame.rootFrame();
		if (formals != null) {
			f = Frame.inner(f);
			final int size = formals.formals.size();
			Thing[] args = size == 0 ? new Thing[]{} : new Thing[] { arguments };
			if (size > 1) {
				Doom.runtime("Top-level spawned components currently must have 0 or 1 argument", originalCompDecl);
			}
			final boolean matchFormals = Matchiste.matchFormals(formals, args, f);
			if (!matchFormals) {
				Doom.runtime("Formal argument doesn't match actual argument.", originalCompDecl, "Formals = " + formals, "\nActuals = " + arguments);
			}
		}
		this.spawnedSiteData = SiteData.start(site, spawnSyn, f, serverSocket);
		
		/*
		if (spawnSyn instanceof Spawn) {
			Spawn spawn = (Spawn) spawnSyn;
			this.spawnedSiteData = SiteData.start(site, spawn, Frame.rootFrame(), serverSocket);
		}
		else if (spawnSyn instanceof ComponentDecl) {
			ComponentDecl compDecl = (ComponentDecl) spawnSyn;
			Frame f = Frame.inner(Frame.rootFrame());
			
			final int size = compDecl.formals.formals.size();
			Thing[] args = size == 0 ? new Thing[]{} : new Thing[] { arguments };
			if (size > 1) {
				Doom.runtime("Top-level spawned components currently must have 0 or 1 argument", compDecl);
			}
			final boolean matchFormals = Matchiste.matchFormals(compDecl.formals, args, f);
			if (matchFormals) {
				Spawn spawn = compDecl.asSpawn();
				this.spawnedSiteData = SiteData.start(site, spawn, f, serverSocket);
			}
			else {
				Doom.runtime("Arguments don't match", compDecl, "Formals = " + compDecl.formals, "\n actuals =" + Bard.sep(args, ", "));
			}
		}
		*/
	}
	
	
	
	public static boolean thereAreErrors() {
		for (CompilerMessage msg : Compilation.current.messages) {
			if (msg.danger == DangerLevel.ERROR) {
				return true;
			}
		}
		return false;
	}



}
