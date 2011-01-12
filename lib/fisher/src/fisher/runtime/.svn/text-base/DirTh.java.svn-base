
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  DirTh extends FileTh  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static DirTh of(String s, Syntax src) throws FisherException {
		File file2 = new File(s);
		if (file2.exists() && !(file2.isDirectory())) {
			Doom.runtime("Not a directory: " + s, src);
		}
		return new DirTh(file2);
	}
	
	/* package*/ static DirTh ofChecked(File f) throws FisherException {
		return new DirTh(f);
	}
	
	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "dir";
	}
	
	private DirTh(File file) throws FisherException{
		super(file);
		// TODO Auto-generated constructor stub
	}
	
	
	private static final int Cfiles = FILE + 1;
	private static final int Cfile = FILE + 2;
	private static final int Cwrite = FILE + 3;
	private static final int Cmkdir = FILE + 4;
	
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("files", Cfiles);
		methodCode.put("file", Cfile);
		methodCode.put("write", Cwrite);
		methodCode.put("writeBytes", Cwrite);
		methodCode.put("setContents", Cwrite);
		methodCode.put("mkdir", Cmkdir);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,   Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case Cfiles: 
				return this.Cfiles(methodName, args, evaller, frame, src);
			case Cfile: 
				return this.Cfile(methodName, args, evaller, frame, src);
			case Cwrite: 
				return this.Cwrite(methodName, args, evaller, frame, src);
			case Cmkdir: 
				return this.Cmkdir(methodName, args, evaller, frame, src);
			default:
				return super.invokeMethod(methodName, args,  src);
			}
		}
		return super.invokeMethod(methodName, args,   src);
	}
	
	
	
	private Thing Cwrite(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		Doom.runtime("Can't write to a directory!", src, this);
		return null;
	}
	
	private Thing Cmkdir(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		file.mkdir();
		return null;
	}

	
	private ListTh Cfiles(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if( file.exists() ) {
			File[] kidnames = file.listFiles();
			List<FileTh> files = Bard.list();
			for (File kid : kidnames) {
				files.add(FileTh.of(kid));
			}
			return ListTh.fromJavaList(files);
		}
		else return null;
	}
	
	private FileTh Cfile(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String kidname = args[0].asString(src);
		File kidfile = new File(file, kidname);
		return FileTh.of(kidfile);
	}
	
	
	
	
}
