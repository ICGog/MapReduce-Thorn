package fisher.runtime.lib.sox;

import java.net.Socket;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.BoolTh;
import fisher.runtime.IntTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.dist.DistUtil;
import fisher.runtime.lib.SiteForStrings;
import fisher.runtime.lib.socketeer.StringSocketThread;
import fisher.syn.core.Syntax;
import fisher.util.FisherException;

public class NetSite extends SiteForStrings {

	public final Thing protocol;
	
	public NetSite(String nodeName, int port, Thing protocol) {
		super(nodeName, port);
		this.protocol = protocol;
	}
	
	public NetSite(Thing nodeNameTh, Thing portTh, Thing protocol) throws FisherException{
		this(nodeNameTh.asString(Evaller.lastSyntax()), portTh.asJavaInt(Evaller.lastSyntax()), protocol);
	}
	
	protected void sendString(String s, Syntax src, Evaller evaller, Framelike frame) throws FisherException {
		String sEncoded;
		if (this.protocol == null) {
			sEncoded = s;
		}
		else {
			Thing encTh = protocol.invokeMethod("encode", new Thing[]{StringTh.of(s)},  src);
			sEncoded = EvalUtil.toString(encTh);
		}
		DistUtil.sendString(sEncoded, this, protocol, src);
	}
	
	public Thing eq(Thing other) {
		if (other instanceof NetSite) {
			NetSite ns = (NetSite) other;
			return BoolTh.of(this.equals(other) && this.protocol == ns.protocol);
		}
		else return BoolTh.False;		
	}
	
	public Thing th_hashcode() {
		return IntTh.of(this.hashCode());
	}
	
	

}
