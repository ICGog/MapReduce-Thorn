package magne;

import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.FisherException;

public class Sleeper extends ThingExtended{

	public static Thing sleep(Thing ms) throws FisherException{
		try{
		Thread.sleep(ms.asLong(null));
		}
		catch(InterruptedException e){
			e.printStackTrace();
		}
		return null;
	}
}

