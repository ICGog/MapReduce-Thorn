
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
//package fisher.statics;
//
//import fisher.eval.EvalUtil;
//import fisher.eval.Evaller;
//import fisher.eval.Frame;
//import fisher.eval.interfaces.Framelike;
//import fisher.runtime.Frameable;
//import fisher.runtime.ModuleInstanceTh;
//import fisher.runtime.Thing;
//import fisher.syn.ImportStmt;
//import fisher.util.Doom;
//import fisher.util.FisherException;
//
//public enum ImportStructure {
//	WHOLE_MODULE() {
//		@Override
//		public Thing eval(ImportStmt syn, Framelike frame, Evaller evaller) throws FisherException {
//			Seal moduleSeal = syn.moduleStatic.moduleSeal();
//			ModuleInstanceTh instance = evaller.getInstantiation(syn.moduleStatic, true, syn);
//			frame.store(moduleSeal, instance, syn);
//			return EvalUtil.nip(syn);
//		}
//	},
//	PWN_WHOLE_MODULE() {
//		@Override
//		public Thing eval(ImportStmt syn, Framelike frame, Evaller evaller) throws FisherException {
//			Seal moduleSeal = syn.asSeal;
//			ModuleInstanceTh instance = evaller.getInstantiation(syn.moduleStatic, false, syn);
//			frame.store(moduleSeal, instance, syn);
//			return EvalUtil.nip(syn);
//		}
//	},
//	ONE_MEMBER() {
//		@Override
//		public Thing eval(ImportStmt syn, Framelike frame, Evaller evaller) throws FisherException {
//			
//			// TODO: Ack! moduleSeal tells whether we're trying ot get
//			// (1) a member out of a possibly-extant shared module instance
//			// (2) a member out of a definitely-extant possibly-shared module
//			// isntance.
//			if (syn.one_member__possibly_extant) {
//				ModuleInstanceTh instance = evaller.getInstantiation(syn.moduleStatic, true, syn);
//				assert (syn.membersToImport.size() == 1);
//				Seal memberSeal = syn.membersToImport.get(0);
//				Frameable frameable = instance.baseValue(memberSeal, syn);
//				frame.store(syn.asSeal, frameable, syn);
//				return EvalUtil.nip(syn);
//			}
//			else {
//				Seal moduleSeal = syn.sealToImportFrom;
//				Frameable moduleInstanceFr = frame.RValue(moduleSeal, syn);
//				if (moduleInstanceFr instanceof ModuleInstanceTh) {
//					ModuleInstanceTh instance = (ModuleInstanceTh) moduleInstanceFr;
//					assert (syn.membersToImport.size() == 1);
//					Seal memberSealOutside = syn.membersToImport.get(0);
//					Seal memberSeal = memberSealOutside.dealias();
//					Frameable frameable = instance.baseValue(memberSeal, syn);
//					frame.store(syn.asSeal, frameable, syn);
//					return EvalUtil.nip(syn);
////					GET THE FIELD OUT OF IT
//				}
//				else {
//					Doom.runtime("Something that had to be a module, wasn't", syn, moduleSeal, moduleInstanceFr, frame, evaller);
//					return null;
//				}
//			}
//		}
//	},
//	MODULE_STAR() {
//		@Override
//		public Thing eval(ImportStmt syn, Framelike frame, Evaller evaller) throws FisherException {
//			Seal moduleSeal = syn.moduleStatic.moduleSeal();
//			ModuleInstanceTh instance = evaller.getInstantiation(syn.moduleStatic, true, syn);
//			for (Seal memberSeal : syn.membersToImport) {
//				Frameable frameable = instance.baseValue(memberSeal, syn);
//				frame.store(memberSeal, frameable, syn);
//			}
//			return EvalUtil.nip(syn);
//		}
//	},
//	SOMETHING_WE_DO_NOT_DO() {
//		@Override
//		public Thing eval(ImportStmt syn, Framelike frame, Evaller evaller) throws FisherException {
//			Doom.internal("Trying to evaluate an import statement of a structure we do not support", syn, frame,
//					evaller);
//			return null;
//		}
//	};
//	public abstract Thing eval(ImportStmt syn, Framelike frame, Evaller evaller) throws FisherException;
//}
