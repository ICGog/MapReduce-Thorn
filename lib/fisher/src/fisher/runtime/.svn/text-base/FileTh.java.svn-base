
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

import java.util.HashMap;
import java.util.Map;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.Security;

import java.io.*;

public  class  FileTh extends ThingBuiltIn  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public File file;

	public static FileTh of(String string) throws FisherException{
		return FileTh.of(new File(string));
	}

	public static FileTh of(File file) throws FisherException{
		return new FileTh(file);
	}

	protected FileTh(File file) throws FisherException {
		super();
		Security.sandbag("no files allowed");
		this.file = file;
	}

	@Override
	public Object unthingify() {
		return file;
	}
	
	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "file";
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		return true;
	}

	public String toString() {
		return file.toString();
	}

	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int Cabs = 2;
	private final static int Cpath = 3;
	private final static int CwriteBytes = 4;
	private final static int Cwriteln = 5;
	private final static int Cdel = 6;
	private final static int Cflush = 7;
	private final static int Cexists = 8;
	private final static int Ccontents = 9;
	private final static int Cclear = 10;
	private final static int Cdirp = 11;
	private final static int Cparent = 12;
	private final static int CasDir = 13;
	private final static int Crename = 14;
	private final static int ChashCode = 15;
	private final static int CreadBytes = 16;
	private final static int Clen = 17;
	private final static int Cname = 18;
	private final static int Cmkdir = 19;

	protected final static int FILE = 100; // DirTh codes are above FILE; FileTh codes are below. 

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("num", Clen);
		methodCode.put("abs", Cabs);
		methodCode.put("path", Cpath);
		methodCode.put("writeBytes", CwriteBytes);
		methodCode.put("setContents", CwriteBytes);
		methodCode.put("setContents", CwriteBytes);
		methodCode.put("writeln", Cwriteln);
		methodCode.put("del!", Cdel);
		methodCode.put("flush", Cflush);
		methodCode.put("exists?", Cexists);
		methodCode.put("contents", Ccontents);
		methodCode.put("clear!", Cclear);
		methodCode.put("dir?", Cdirp);
		methodCode.put("parent", Cparent);
		methodCode.put("asDir", CasDir);
		methodCode.put("rename", Crename);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("readBytes", CreadBytes);
		methodCode.put("len", Clen);
		methodCode.put("name", Cname);
		methodCode.put("mkdir", Cmkdir);

	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof FileTh) {
			FileTh other = (FileTh) obj;
			return other.file.equals(this.file);
		} else
			return false;
	}

	@Override
	public int hashCode() {
		return file.hashCode();
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,   Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case NUM:
				return this.tryThingMethods(methodName, args,   src);
			case Cabs:
				return this.Cabs(methodName, args, evaller, frame, src);
			case Cpath:
				return this.Cpath(methodName, args, evaller, frame, src);
			case CwriteBytes:
				return this.CwriteBytes(methodName, args, evaller, frame, src);
			case Cwriteln:
				return this.Cwriteln(methodName, args, evaller, frame, src);
			case Cdel:
				return this.Cdel(methodName, args, evaller, frame, src);
			case Cflush:
				return this.Cflush(methodName, args, evaller, frame, src);
			case Cexists:
				return this.Cexists(methodName, args, evaller, frame, src);
			case Ccontents:
				return this.Ccontents(methodName, args, evaller, frame, src);
			case Cclear:
				return this.Cclear(methodName, args, evaller, frame, src);
			case Cdirp:
				return this.Cdirp(methodName, args, evaller, frame, src);
			case Cparent:
				return this.Cparent(methodName, args, evaller, frame, src);
			case CasDir:
				return this.CasDir(methodName, args, evaller, frame, src);
			case Crename:
				return this.Crename(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case CreadBytes:
				return this.CreadBytes(methodName, args, evaller, frame, src);
			case Clen:
				return this.Clen(methodName, args, evaller, frame, src);
			case Cname:
				return this.Cname(methodName, args, evaller, frame, src);
			case Cmkdir:
				return this.Cmkdir(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,  src);
	}
	
	private Thing Cname(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		final String name = this.file.getName();
		return name == null ? null : StringTh.of(name);		
	}

	private Thing Clen(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (file.exists()) {
			return IntTh.of(file.length());
		}
		else return null;		
	}
	private Thing Crename(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		File dest;
		if (args[0] instanceof FileTh) {
			dest = ((FileTh) args[0]).file;
		} else if (args[0].isString()) {
			dest = new File(args[0].asString(src));
		} else {
			Doom.runtime("rename expects a file or a string as the new name.", src, args[0]);
			return null;
		}

		return BoolTh.of(file.renameTo(dest));

	}

	private Thing Cwriteln(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		try {
			String s = EvalUtil.toString(args[0]);
			if (!s.endsWith("\n")) {
				s += "\n";
			}
			write(s);
			return BoolTh.True;
		} catch (IOException e) {
			return null;
		}
	}

	private Thing CwriteBytes(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		try {
			byte[] s = EvalUtil.asBytes(args[0], src);
			writeBytes(s);
			return BoolTh.True;
		} catch (IOException e) {
			return null;
		}
	}

	private Thing CreadBytes(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		try {
			checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
			FileInputStream fis = new FileInputStream(this.file);
			int len = (int) file.length();
			byte[] b = new byte[len];
			fis.read(b);
			BytesTh bytes = new BytesTh(b);
			return bytes;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	private void writeBytes(byte[] s) throws IOException {
		//		FileWriter fw = new FileWriter(file);
		//		fw.write(s);
		//		fw.flush();
		//		fw.close();

		FileOutputStream fos = new FileOutputStream(file, true);
		fos.write(s);
		fos.flush();
		fos.close();
	}

	private void write(String s) throws IOException {
		FileOutputStream fos = new FileOutputStream(file, true);
		Writer wr = (new OutputStreamWriter(fos, Bard.MY_FAVORITE_ENCODING));
		wr.write(s);
		wr.flush();
		wr.close();
	}
	
	private Thing Cmkdir(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		file.mkdir();
		return null;
	}

	private void setContents(String s) throws IOException {
		FileWriter fw = new FileWriter(file);
		fw.write(s);
		fw.flush();
		fw.close();
	}

	private DirTh CasDir(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (file.isDirectory()) {
			return DirTh.ofChecked(file);
		} else
			return null;
	}

	private FileTh Cparent(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		File parentfi = file.getParentFile();
		if (parentfi == null)
			return null;
		else
			return FileTh.of(parentfi);
	}

	private BoolTh Cdirp(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return BoolTh.of(file.isDirectory());
	}

	private BoolTh Cclear(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		try {
			setContents("");
			return BoolTh.True;
		} catch (Exception e) {
			// TODO: handle exception
			return BoolTh.False;
		}
	}

	private StringTh Ccontents(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (file.exists()) {
			String s = Bard.contentsOf(file);
			return StringTh.of(s);
		} else
			return null;
	}

	private BoolTh Cexists(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return file == null ? BoolTh.False : BoolTh.of(file.exists());
	}

	private Thing Cflush(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		// Currently a no-op.
		return null;
	}

	private Thing Cdel(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		boolean b = file.delete();
		return BoolTh.of(b);
	}

	private StringTh Cpath(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		try {
			return StringTh.of(file.getCanonicalPath());
		} catch (IOException e) {
			return null;
		}
	}

	private StringTh Cabs(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return StringTh.of(file.getAbsolutePath());
	}

}
