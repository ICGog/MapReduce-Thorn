package fisher.runtime.dist;

import java.util.HashMap;
import java.util.Map;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.Thing;
import fisher.runtime.ThingBuiltIn;
import fisher.runtime.ThingExtended;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;


public abstract class AbstractLetter extends ThingBuiltIn implements Letter {
	

	public abstract Thing securityInfo();
	
	private final static int STR = 0;
	private final static int NUM = 1;
	private final static int CsecurityInfo = 1;
	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("str", STR);
		methodCode.put("len", NUM);
		methodCode.put("num", NUM);
		methodCode.put("securityInfo", CsecurityInfo);
	}
	@Override
	public Thing invokeMethod(String methodName, Thing[] args,  Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case CsecurityInfo:
				return this.securityInfo();
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args, src);
	}
}
