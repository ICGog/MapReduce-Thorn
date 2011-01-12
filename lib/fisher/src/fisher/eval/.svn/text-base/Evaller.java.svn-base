
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.interfaces.Framelike;
import fisher.runtime.*;
import fisher.runtime.dist.ComponentThread;
import fisher.statics.ModuleStatic;
import fisher.statics.SealForClass;
import fisher.statics.purity.PurityStatus;
import fisher.syn.Cmd;
import fisher.syn.Formals;
import fisher.syn.Pat;
import fisher.syn.core.Syntax;
import fisher.test.TestUtils;
import fisher.util.*;

public  class  Evaller  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	private static ThreadLocal<Evaller> evallerPerThread = new ThreadLocal<Evaller>();
	
	public Map<String, ClassDynamicTh> loadedSharedClasses = new HashMap<String, ClassDynamicTh>();
	
	public static Evaller mine() {
		Evaller evaller = evallerPerThread.get();
		if (evaller == null){
			String threadNote = "(not a ComponentThread)";
			if (Thread.currentThread() instanceof ComponentThread) {
				ComponentThread ct = (ComponentThread) Thread.currentThread();
				threadNote = "" + ct.component;
			}
			Doom.internalCatastrophe("No evaller for this thread!", null, Thread.currentThread(),
					TestUtils.testScribble, "\n threadNote = " + threadNote + "\n\n\n\n"
					);
			
		}
		return evaller;
	}
	
	private static ThreadLocal<Syntax> currentSyntaxPerThread = new ThreadLocal<Syntax>();
	
	public static Syntax lastSyntax() {
		return currentSyntaxPerThread.get();
	}
	
	public static void setLastSyntax(Syntax syntax) {
		currentSyntaxPerThread.set(syntax);
	}
	
	public static void register(Evaller evaller) {
		if (evallerPerThread.get() != null) {
			Doom.internalCatastrophe("An evaller is already registered for this thread!", null, Thread.currentThread());
		}
		evallerPerThread.set(evaller);
	}
	
	public static void reset() {
		evallerPerThread = new ThreadLocal<Evaller>();
	}
	
	public static Thing fullEval(Cmd syn) throws FisherException {
		Evaller evaller = of(syn);
		Thing res = evaller.eval(syn, evaller.rootFrame);
		return res;
	}

	public static Evaller of(Cmd syn) throws FisherException {
		// TODO: frames no longer hold evallers, so this isn't necessary.
		Frame rootFrame = Frame.rootFrame();
		Evaller evaller = new Evaller(syn, rootFrame); // which fills it in.
		return evaller;
	}
	
	public Thing eval(Cmd syn, Framelike frame) throws FisherException {
//		Syntax oldAt = Doom.maybeAt;
//		Doom.at(syn);
		try {
			Evaller.setLastSyntax(syn);
			Thing result = syn.accept(computer, frame);
//		Doom.at(oldAt);
			return result;
		}
		catch(FisherException fe) {
			throw fe;
		}
		catch(Exception ex) {
			throw new FisherRuntimeException(ex.toString());
		}
	}
	

	public Thing[] evalAll(List<? extends Cmd> cmds, Framelike frame) throws FisherException {
		return evalAllWithExtraSpace(cmds, frame, 0);
	}
	
	public Thing[] evalAllWithExtraSpace(List<? extends Cmd> cmds, Framelike frame, int extras) throws FisherException {
		Thing[] results = new Thing[cmds.size() + extras];
		int i = 0;
		for (Cmd cmd : cmds) {
			Thing res = this.eval(cmd, frame);
			results[i++] = res;
		}
		return results;
	}
	public Thing[] evalAllButFirst(List<? extends Cmd> cmds, Framelike frame) throws FisherException {
		Thing[] results = new Thing[cmds.size()-1];
		int i = 0;
		boolean first = true;
		for (Cmd cmd : cmds) {
			if (first) {
				first = false;
			}
			else {
				Thing res = this.eval(cmd, frame);
				results[i++] = res;
			}
		}
		return results;
	}
	
	private Map<ModuleStatic, ModuleDynamicTh> sharedModuleInstances = new HashMap<ModuleStatic, ModuleDynamicTh>();
	
	public ModuleDynamicTh getInstantiation(ModuleStatic moduleStatic, boolean useShared, Syntax src) throws FisherException {
		if (useShared) {
			if (sharedModuleInstances.containsKey(moduleStatic)) {
				return sharedModuleInstances.get(moduleStatic);
			}
			else {
				Frame frame = Frame.rootFrame();
				ModuleDynamicTh newInstance = moduleStatic.instantiate(this, src, frame);
				sharedModuleInstances.put(moduleStatic, newInstance);
				recordSharedClassDynamics(newInstance);
				return newInstance;				
			}
		}
		else /* not shared */ {
			Frame frame = Frame.rootFrame();
			ModuleDynamicTh newInstance = moduleStatic.instantiate(this, src, frame);
			// Don't save it though.
			return newInstance;			
		}
	}
	
	private synchronized void  recordSharedClassDynamics(ModuleDynamicTh module) throws FisherException{
		for (SealForClass seal : module.moduleStatic.sealsOfClasses) {
			ClassDynamicTh cd = (ClassDynamicTh) module.fetchImmediateMemberR(seal, null);
			if (cd.classStatic.purityStatus() != PurityStatus.PURE) continue;
			String key = cd.classStatic.serializationKey();
			loadedSharedClasses.put(key, cd);	
//			System.err.println("Yggdorff! I, " + this.hashCode() + ", now know of class " + key);
		}
	}
	
	public synchronized ClassDynamicTh classDynamicForKey(String key) {
		return loadedSharedClasses.get(key);
	}
	
	/**
	 * @param formals
	 * @param args
	 * @param frame
	 * @return true if <code>args</code> are suitable for <code>formals</code> (and in this case the bindings are definitely done.)
	 *   False if the args are not suitable.  
	 *   TODO: ought to undo bindings if the match is not made.
	 * @throws FisherException
	 */
	public boolean evalFormals(Formals formals, Thing[] args, Framelike frame) throws FisherException {
		if (formals.formals.size() != args.length) {
			return false;
		}
		for(int i = 0; i < args.length; i++ ) {
			Pat pat = formals.formals.get(i);
			Thing subject = args[i];
			boolean matches = Matchiste.match(pat, subject, frame);
			if (! matches) {
				// TODO -- undo all the match bindings.
				return false;
			}
		}
		return true;
	}
	

	public final Syntax wholeSrc;
	public final Frame rootFrame;
	public final Computer computer;
	
	public /* but don't use unless you know what you're doing*/ Evaller(Syntax wholeSrc, Frame rootFrame) {
		super();
		this.wholeSrc = wholeSrc;
		this.rootFrame = rootFrame;
		this.computer = new Computer(this);
		register(this);
	} 
	
	
	
}
