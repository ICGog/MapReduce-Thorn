
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics;

import fisher.eval.EvalUtil;
import fisher.runtime.AbstractRangeTh;
import fisher.runtime.BoolTh;
import fisher.runtime.BytesTh;
import fisher.runtime.CharTh;
import fisher.runtime.ClosureTh;
import fisher.runtime.FileTh;
import fisher.runtime.FloatTh;
import fisher.runtime.IntTh;
import fisher.runtime.ListTh;
import fisher.runtime.Nullity;
import fisher.runtime.OrdTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.TableTh;
import fisher.runtime.Thing;
import fisher.util.Doom;

public enum BuiltInTypeEnum {
	INT("int"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof IntTh;
		}
	},
	FLOAT("float"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof FloatTh;
		}
	},
	BOOL("bool"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof BoolTh;
		}
	},
	CHAR("char"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof CharTh;
		}
	},
	STRING("string"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof StringTh;
		}
	},
	LIST("list"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof ListTh;
		}
	},
	RECORD("record"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof RecordTh;
		}
	},
	RANGE("range"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof AbstractRangeTh;
		}
	},
	TABLE("table")
	{
		@Override
		public boolean is(Thing thing) {
			return thing instanceof TableTh;
		}
	},
	BYTES("bytes"){
		@Override
		public boolean is(Thing thing) {
			return thing instanceof BytesTh;
		}
	},
	FILE("file")
	{
		@Override
		public boolean is(Thing thing) {
			return thing instanceof FileTh;
		}
	},
	DIR("dir")
	{
		@Override
		public boolean is(Thing thing) {
			return thing instanceof fisher.runtime.DirTh;
		}
	},
	FN("fn")
	{
		@Override
		public boolean is(Thing thing) {
			return thing instanceof ClosureTh;
		}
	},
	NULLITY("nullity")
	{
		@Override
		public boolean is(Thing thing) {
			return thing instanceof Nullity;
		}
	},
	ORD("ord")
	{
		@Override
		public boolean is(Thing thing) {
			return thing instanceof OrdTh;
		}
	},
	PURE("pure")
	{
		@Override
		public boolean is(Thing thing) {
			return EvalUtil.isPure(thing);
		}
	}
	;
	
	public String name;

	private BuiltInTypeEnum(String name) {
		this.name = name;
	}
	public String toString() {
		return name;
	}
	
	public abstract boolean is(Thing thing) ;
}
