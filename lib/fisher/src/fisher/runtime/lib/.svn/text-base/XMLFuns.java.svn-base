
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime.lib;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import fisher.eval.Evaller;
import fisher.runtime.ListTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  XMLFuns  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	
	public static final DocumentBuilderFactory docBldFact = DocumentBuilderFactory.newInstance();
	private static DocumentBuilder docBldr = null; 
	
	public static DocumentBuilder docBldr() {
		try {
			if (docBldr == null) docBldr = docBldFact.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		return docBldr;
	}
	
	public static Thing parse(Thing source) throws FisherException {
		Document doc;
		try {
			if (docBldr == null) {
				docBldr = docBldFact.newDocumentBuilder();
			}
			if (source instanceof StringTh) {
				StringTh sth = (StringTh) source;
				String s = sth.value;
				doc = docBldr.parse(new ByteArrayInputStream(s.getBytes()));
			} else if (source instanceof File) {
				File thfi = (File) source;
				doc = docBldr.parse(thfi.file);
			} else {
				Doom.runtime("XMLFuns.parse requires a string or a File argument, not a " + source.typeString(), Evaller.lastSyntax());
				doc = null; // No, it just fails.
			}
		} catch (ParserConfigurationException e) {
			Doom.runtime("XML Parser configuration error " + e, null);
			doc = null;
		} catch (SAXException e) {
			Doom.runtime("SAX exception: " + e, null);
			doc = null;
		} catch (IOException e) {
			Doom.runtime("I/O exception: " + e, null);
			doc = null;
		}
		
		return thingify(doc.getDocumentElement());
	}
	
	public static Thing thingify(Node n) throws FisherException {
		if (n instanceof Element) {
			Element el = (Element) n;
			String stag = el.getNodeName();
			StringTh tag = StringTh.of(stag);
			RecordTh attrs = RecordTh.unfinished();
			NamedNodeMap attributes = el.getAttributes();
			int nat = attributes.getLength();
			for(int i = 0; i < nat; i++){
				Node item = attributes.item(i);
				attrs.setField(item.getNodeName(), StringTh.of(item.getNodeValue()));
			}
			attrs.finish();
			ListTh children = ListTh.EMPTY;
			for(Node kid = el.getFirstChild(); kid != null; kid = kid.getNextSibling()) {
				Thing kidthing = thingify(kid);
				children = children.cons(kidthing);
			}
			children = children.reversed();
			return new Elem(tag, attrs, children);
		}
		else if (n instanceof Text) {
			Text text = (Text) n;
			String s = text.getNodeValue();
			StringTh sth = StringTh.of(s);
			return sth;
		}
		else {
			Doom.runtime("I don't know how to convert this into Thorn-XML: " + n, null, n);
			return null;
		}
	}
	
	
	
	
	public static TransformerFactory tfactory = TransformerFactory.newInstance();
	
	public static final String defaultIndent = "yes";
	//public static final String defaultIndentAmount = "2";
	//public static final String INDENT_AMOUNT = "{http://xml.apache.org/xslt}indent-amount";
	public static final String omit = "yes";
	
	public static void serialize(Document doc, OutputStream out) throws Exception {
	        Transformer serializer;
	        try {
	            serializer = tfactory.newTransformer();
	            //Setup indenting to "pretty print".  Please set Elem.str(ctrl) to do the same thing.
	            serializer.setOutputProperty(OutputKeys.INDENT, XMLFuns.defaultIndent);
				serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, XMLFuns.omit);
	            
	            serializer.transform(new DOMSource(doc), new StreamResult(out));
	        } catch (TransformerException e) {
	            // this is fatal, just dump the stack and throw a runtime exception
	            e.printStackTrace();
	            
	            throw new RuntimeException(e);
	        }
	    }
	
}
