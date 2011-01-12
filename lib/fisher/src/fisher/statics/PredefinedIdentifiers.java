
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

import java.util.HashSet;

import fisher.runtime.BuiltInType;
import fisher.runtime.Frameable;
import fisher.runtime.Thing;
import fisher.runtime.builtInFun.*;

public  class  PredefinedIdentifiers  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static final java.util.Set<String> PURE_PREDEFS = new HashSet<String>();
	
	public static final String SAME_TYPE_AND_ALL_FIELDS_EQUAL = "same_type_and_all_fields_equal?";
	public static final Predef[] predefs = 
		new Predef[]{
		predef("int", new SealBuiltInType(BuiltInTypeEnum.INT), new BuiltInType(BuiltInTypeEnum.INT)),
		predef("bool", new SealBuiltInType(BuiltInTypeEnum.BOOL), new BuiltInType(BuiltInTypeEnum.BOOL)),
		predef("char", new SealBuiltInType(BuiltInTypeEnum.CHAR), new BuiltInType(BuiltInTypeEnum.CHAR)),
		predef("string", new SealBuiltInType(BuiltInTypeEnum.STRING), new BuiltInType(BuiltInTypeEnum.STRING)),
		predef("record", new SealBuiltInType(BuiltInTypeEnum.RECORD), new BuiltInType(BuiltInTypeEnum.RECORD)),
		predef("table", new SealBuiltInType(BuiltInTypeEnum.TABLE), new BuiltInType(BuiltInTypeEnum.TABLE)),
		predef("list", new SealBuiltInType(BuiltInTypeEnum.LIST), new BuiltInType(BuiltInTypeEnum.LIST)),
		predef("float", new SealBuiltInType(BuiltInTypeEnum.FLOAT), new BuiltInType(BuiltInTypeEnum.FLOAT)),
		predef("range", new SealBuiltInType(BuiltInTypeEnum.RANGE), new BuiltInType(BuiltInTypeEnum.RANGE)),
		predef("ord", new SealBuiltInType(BuiltInTypeEnum.ORD), new BuiltInType(BuiltInTypeEnum.ORD)),
		predef("file", new SealBuiltInType(BuiltInTypeEnum.FILE), new BuiltInType(BuiltInTypeEnum.FILE)),
		predef("dir", new SealBuiltInType(BuiltInTypeEnum.DIR), new BuiltInType(BuiltInTypeEnum.DIR)),
		predef("nullity", new SealBuiltInType(BuiltInTypeEnum.NULLITY), new BuiltInType(BuiltInTypeEnum.NULLITY)),
		predef("fn", new SealBuiltInType(BuiltInTypeEnum.FN), new BuiltInType(BuiltInTypeEnum.FN)),
		predef("pure", new SealBuiltInType(BuiltInTypeEnum.PURE), new BuiltInType(BuiltInTypeEnum.PURE)),
		predef("bytes", new SealBuiltInType(BuiltInTypeEnum.BYTES), new BuiltInType(BuiltInTypeEnum.BYTES)),
		predef("thisComp", SealMaker.ofPredefFun("thisComp"), ThisCompBIF.class),
		predef("typeName", SealMaker.ofPredefFun("typeName"), TypeNameBIF.class),
		predef("thisSite", SealMaker.ofPredefFun("thisSite"), ThisSiteBIF.class),
		predef("site", SealMaker.ofPredefFun("site"), SiteBIF.class),
		predef("http", SealMaker.ofPredefFun("http"), HttpBIF.class),
		predef("newNonce", SealMaker.ofPredefFun("newNonce"), NewNonceBIF.class),
		predef("syncReply", SealMaker.ofPredefFun("syncReply"), SyncReplyBIF.class),
		predef("splitSync", SealMaker.ofPredefFun("splitSync"), SplitSyncBIF.class, false),
		predef("parseConstant", SealMaker.ofPredefFun("parseConstant"), ParseConstantBIF.class),
		predef("readln", SealMaker.ofPredefFun("readln"), ReadlnBIF.class, false),
		predef("println", SealMaker.ofPredefFun("println"), PrintlnBIF.class, false),
		predef("crude_serialize", SealMaker.ofPredefFun("crude_serialize"), SerializeBIF.class),
		predef("crude_deserialize", SealMaker.ofPredefFun("crude_deserialize"), DeserializeBIF.class),
		predef("command_line_args", SealMaker.ofPredefFun("command_line_args"), Command_line_argsBIF.class),
		predef("argv", SealMaker.ofPredefFun("argv"), Command_line_argsBIF.class),
		predef(PredefinedIdentifiers.SAME_TYPE_AND_ALL_FIELDS_EQUAL, SealMaker.ofPredefFun(SAME_TYPE_AND_ALL_FIELDS_EQUAL), Same_type_and_All_fields_equal_BIF.class),
	};
	
	

	
	public static Predef predef(String name, Seal seal, Class cls, boolean pure) {
		return new Predef(name, seal, cls, null, false);
	}
	public static Predef predef(String name, Seal seal, Class cls) {
		return new Predef(name, seal, cls, null, true);
	}
	
	
	public static Predef predef(String name, Seal seal, Thing thing, boolean pure) {
		return new Predef(name, seal, null, thing, pure);
	}
	public static Predef predef(String name, Seal seal, Thing thing) {
		return new Predef(name, seal, null, thing, true);
	}
	
	public static class Predef {
		public String name;
		public Seal seal;
		public Class cls;
		public Thing val; 
		private Predef(String name, Seal seal, Class cls, Thing thing, boolean pure) {
			super();
			this.name = name;
			this.seal = seal;
			this.cls = cls;
			this.val = thing;
			if (pure) PURE_PREDEFS.add(name);
		}
	}
	
	
	
}
