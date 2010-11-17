package magne;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.apache.commons.fileupload.MultipartStream;

import fisher.runtime.BytesTh;
import fisher.runtime.ListTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.util.FisherException;

public class MimeMultipartParser extends ThingExtended {

	public static Thing parseMultipart(Thing contentTypeThing, Thing buffer) throws FisherException, IOException{
		String contentType = contentTypeThing.toString();
		int boundaryIndex = contentType.indexOf("boundary=");
		byte[] boundary = (contentType.substring(boundaryIndex + 9)).getBytes();

		byte[] bytes = ((BytesTh)buffer).asBytes(null);
		ByteArrayInputStream input = new ByteArrayInputStream(bytes);

		ListTh list = ListTh.EMPTY;

		try{
			MultipartStream multipartStream = new MultipartStream(input, boundary);

			boolean nextPart = multipartStream.skipPreamble();
			while(nextPart) {
				StringTh headers = StringTh.of(multipartStream.readHeaders());

				ByteArrayOutputStream output = new ByteArrayOutputStream();
				multipartStream.readBodyData(output);

				list = list.cons(new BytesTh(output.toByteArray()));
				list = list.cons(headers);					

				nextPart = multipartStream.readBoundary();
			}		
		}
		catch(MultipartStream.MalformedStreamException e) {
			e.printStackTrace();
		}
		return list;
	}
}
