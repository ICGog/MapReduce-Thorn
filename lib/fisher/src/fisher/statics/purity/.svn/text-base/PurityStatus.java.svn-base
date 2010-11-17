
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.statics.purity;

import fisher.util.Doom;

public enum PurityStatus {
	PURE{
		@Override
		public PurityStatus and(PurityStatus p) {
			switch(p) {
			case PURE: return PURE;
			case IMPURE: return IMPURE;
			case INNOCENT: return PURE;
			case ERROR: return ERROR;
			case UNCHECKED: return UNCHECKED;
			case INNOCENT_TIL_PROVEN : return PURE;
			default: Doom.internalCatastrophe("PurityStatus operation is not updated to handle new PurityStatus!", null);return null;
			}
		}
		@Override
		public boolean canBeInPureObject() {
			return true;
		}
	},
	IMPURE{
		@Override
		public PurityStatus and(PurityStatus p) {
			switch(p) {
			case PURE: return IMPURE;
			case IMPURE: return IMPURE;
			case INNOCENT: return IMPURE;
			case ERROR: return ERROR;
			case UNCHECKED: return UNCHECKED;
			case INNOCENT_TIL_PROVEN : return INNOCENT;
			default: Doom.internalCatastrophe("PurityStatus operation is not updated to handle new PurityStatus!", null);return null;
			}
		}
		@Override
		public boolean canBeInPureObject() {
			return false;
		}
	},
	INNOCENT{
		@Override
		public PurityStatus and(PurityStatus p) {
			switch(p) {
			case PURE: return PURE;
			case IMPURE: return IMPURE;
			case INNOCENT: return INNOCENT;
			case ERROR: return ERROR;
			case UNCHECKED: return UNCHECKED;
			case INNOCENT_TIL_PROVEN : return INNOCENT;
			default: Doom.internalCatastrophe("PurityStatus operation is not updated to handle new PurityStatus!", null);return null;
			}
		}
		@Override
		public boolean canBeInPureObject() {
			return true;
		}
	},
	INNOCENT_TIL_PROVEN{
		@Override
		public PurityStatus and(PurityStatus p) {
			switch(p) {
			case PURE: return PURE;
			case IMPURE: return IMPURE;
			case INNOCENT: return INNOCENT;
			case ERROR: return ERROR;
			case UNCHECKED: return UNCHECKED;
			case INNOCENT_TIL_PROVEN : return INNOCENT_TIL_PROVEN;
			default: Doom.internalCatastrophe("PurityStatus operation is not updated to handle new PurityStatus!", null);return null;
			}
		}
		@Override
		public boolean canBeInPureObject() {
			return false;
		}
	},
	ERROR{
		@Override
		public PurityStatus and(PurityStatus p) {
			switch(p) {
			case PURE: return ERROR;
			case IMPURE: return ERROR;
			case INNOCENT: return ERROR;
			case ERROR: return ERROR;
			case UNCHECKED: return ERROR;
			case INNOCENT_TIL_PROVEN : return ERROR;
			default: Doom.internalCatastrophe("PurityStatus operation is not updated to handle new PurityStatus!", null);
				return null;
			}
		}
		@Override
		public boolean canBeInPureObject() {
			return false;
		}
	},
	UNCHECKED{
		@Override
		public PurityStatus and(PurityStatus p) {
			switch(p) {
			case PURE: return UNCHECKED;
			case IMPURE: return UNCHECKED;
			case INNOCENT: return UNCHECKED;
			case ERROR: return ERROR;
			case UNCHECKED: return UNCHECKED;
			case INNOCENT_TIL_PROVEN : return UNCHECKED;
			default: Doom.internalCatastrophe("PurityStatus operation is not updated to handle new PurityStatus!", null);return null;
			}
		}
		@Override
		public boolean canBeInPureObject() {
			return false;
		}
	}
	;
	
	/**
	 * If a class/etc has one parent/import of purity status 'this', and another of 'p', the 
	 * class's purity status is (at most) this.and(p).
	 * 
	 * 
	 * @param p
	 * @return
	 */
	public abstract PurityStatus and(PurityStatus p);
	
	public abstract boolean canBeInPureObject();
	
}
