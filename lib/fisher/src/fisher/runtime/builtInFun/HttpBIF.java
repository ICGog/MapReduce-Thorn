package fisher.runtime.builtInFun;

import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BuiltInFunctionTh;
import fisher.runtime.HttpTh;
import fisher.runtime.Thing;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public class HttpBIF extends BuiltInFunctionTh {

	@Override
	public Thing apply(Thing[] args, Framelike ignoredFrame, Evaller evaller,
			Syntax src) throws FisherException {
		checkNumberOfArgs(0, 0, "()", args, evaller, ignoredFrame, src);
		return new HttpTh();
	}

}
