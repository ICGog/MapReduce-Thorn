package magne;


import fisher.runtime.Thing;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.runtime.dist.Letter;

public class MsgSimpleHttpRequest extends Letter {

	public final SimpleHTTPRequest request;

	public MsgSimpleHttpRequest(SimpleHTTPRequest request){
		super();
		this.request = request;
	}
	
	@Override
	public Thing contents() {
		// TODO Auto-generated method stub
		return request;
	}

	@Override
	public Thing sender() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String toStringWithoutContents() {
		// TODO Auto-generated method stub
		return "http-msg";
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "MsgSimpleHttpRequest";
	}

	
}
