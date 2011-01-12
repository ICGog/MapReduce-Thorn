
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

import java.util.List;

import fisher.util.Bard;

public enum SealKind {
	VAR,
	VAL,
	CLASS,
	ANON_OBJ_CLASS,
	FUN,
	METHOD,
	MODULE,
	PROBE,
	PAT_SEAL,
	PAT_FORMAL,
	FIELD_REF, // a one-off field reference.
	METHOD_REF, // a one-off method reference.
	PAT_REF, // a one-off pat reference
	LOOP_NAME, 
	FIELD_NAME, // a one-off field name, as in {: a:1 :}
	IT, // a seal for a pattern non-variable
	BUILT_IN_TYPE, // int, boolean, etc.
	COLUMN, // table column
	ASYNC,
	SYNC,
	SPAWN, // name of a spawn
	DEAD, // id in dead code
	JAVALY_FUN,
	JAVALY_CLS,
	BEING_TESTED
	;
	
	
	
	public static final List<SealKind> shouldBeAddedToEnvOfSubclass = Bard.list(VAR, VAL);
	
	public static final List<SealKind> suitableForSpawnedComponent = Bard.list(VAL, CLASS, BUILT_IN_TYPE, MODULE);
	public static final List<SealKind> shouldBeImportedOnSpawning = Bard.list(VAL, CLASS, BUILT_IN_TYPE);
}
