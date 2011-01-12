package magne;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;

import javax.swing.text.StyledEditorKit.BoldAction;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.xml.sax.SAXException;

import fisher.runtime.BoolTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;

public class XHTMLChecker extends ThingExtended {
	
	//String xhtml1_strict = "http://www.w3.org/2002/08/xhtml/xhtml1-strict.xsd";
	
	Validator validator;
	String message;
	
	public XHTMLChecker(Thing schemaStr) throws SAXException{
		SchemaFactory factory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
		
		//URL schemaLocation = new URL(xhtml1_strict);
		//File schemaLocation = new File("schemas" + File.separator + "xhtml1-strict.xsd");
		
		StringReader reader = new StringReader(schemaStr.toString());
		
		Schema schema = factory.newSchema(new StreamSource(reader));
		validator = schema.newValidator();
	}
	
	public Thing validate(Thing html) throws SAXException, IOException{
		Source source = new StreamSource(new StringReader(html.toString()));
		try {
			validator.validate(source);
			return BoolTh.True;
		} catch (SAXException ex) {
			message =  ex.getMessage();
			return BoolTh.False;
		}
	}
	
	public Thing message(){
		return StringTh.of(message);
	}
}
