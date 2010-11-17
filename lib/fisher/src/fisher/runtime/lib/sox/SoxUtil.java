package fisher.runtime.lib.sox;

import fisher.eval.Computer;
import fisher.eval.Evaller;
import fisher.runtime.Thing;
import fisher.util.FisherException;

public class SoxUtil {
	public static String inputStyle(Thing protocol)  {
		try {
			if (protocol == null) return "LINE";
			Thing isTh = protocol.invokeMethod("input", Computer.NO_ARGS, Evaller.lastSyntax());
			if (isTh == null) return "HTTP";
			return isTh.toString();
		} catch (Exception e) {
			e.printStackTrace();
			return "LINE";
		}
	}

}
