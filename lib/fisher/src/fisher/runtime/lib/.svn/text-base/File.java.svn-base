
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.lib;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.FileTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;
import fisher.util.Security;

public  class  File extends ThingExtended  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	public final java.io.File file;

	public File(Thing f) throws FisherException {
		Security.sandbag("No files allowed.");
		if (f instanceof StringTh) {
			StringTh sf = (StringTh) f;
			this.file = new java.io.File(sf.toString());
		} else {
			Doom.runtime("File requires a string argument", ThingExtended.mySrc(), "");
			this.file = null;
		}
	}

	public static File of(java.io.File f) throws FisherException{
		return new File(f);
	}

	private File(java.io.File f) throws FisherException {
		this.file = f;
	}
	
	public String toString() {
		return this.file.toString();
	}

//	public StringTh str() {
//		return StringTh.of(this.file.toString());
//	}

	public Thing abs() {
		return StringTh.of(file.getAbsolutePath());
	}

	public Thing path() {
		try {
			return StringTh.of(file.getCanonicalPath());
		} catch (IOException e) {
			return null;
		}
	}

	public Thing rename(Thing newName) throws FisherException {
		Syntax src = ThingExtended.mySrc();
		java.io.File dest;
		if (newName instanceof File) {
			dest = ((File) newName).file;
		} else if (newName.isString()) {
			dest = new java.io.File(newName.asString(src));
		} else {
			Doom.runtime("rename expects a file or a string as the new name.", src, newName);
			return null;
		}
		return BoolTh.of(file.renameTo(dest));
	}

	public Thing writeln(Thing obj) throws FisherException {
		try {
			String s = EvalUtil.toString(obj);
			if (!s.endsWith("\n")) {
				s += "\n";
			}
			write(s);
			return BoolTh.True;
		} catch (IOException e) {
			return null;
		}
	}

	public void write(String s) throws IOException {
		FileOutputStream fos = new FileOutputStream(file, true);
		Writer wr = (new OutputStreamWriter(fos));
		wr.write(s);
		wr.flush();
		wr.close();
	}

	public void setContents(String s) throws IOException {
		FileWriter fw = new FileWriter(file);
		fw.write(s);
		fw.flush();
		fw.close();
	}

	public Thing writeBytes(Thing inscription) throws FisherException {
		try {
			String s = EvalUtil.toString(inscription);
			writeBytes(s);
			return BoolTh.True;
		} catch (IOException e) {
			return null;
		}
	}

	private void writeBytes(String s) throws IOException {
		FileOutputStream fos = new FileOutputStream(file, true);
		fos.write(s.getBytes());
		fos.flush();
		fos.close();
	}

	public Thing readBytes()
			throws FisherException {
		try {
			FileInputStream fis = new FileInputStream(this.file);
			int len = (int) file.length();
			byte[] b = new byte[len];
			fis.read(b);
			String sb = new String(b);
			return StringTh.of(sb);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public Thing parent() throws FisherException {
		java.io.File parentfi = file.getParentFile();
		if (parentfi == null)
			return null;
		else
			return File.of(parentfi);
	}

	public BoolTh dirp() throws FisherException {
		return BoolTh.of(file.isDirectory());
	}

	public BoolTh clear() throws FisherException {
		try {
			setContents("");
			return BoolTh.True;
		} catch (Exception e) {
			// TODO: handle exception
			return BoolTh.False;
		}
	}

	public StringTh contents() throws FisherException {
		if (file.exists()) {
			String s = Bard.contentsOf(file);
			return StringTh.of(s);
		} else
			return null;
	}

	public BoolTh exists() throws FisherException {
		return BoolTh.of(file.exists());
	}

	public Thing flush() throws FisherException {
		// currently a no-op, since files are left closed.
		return null;
	}

	public Thing del() throws FisherException {
		boolean b = file.delete();
		return BoolTh.of(b);
	}

}
