
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

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import fisher.eval.EvalUtil;
import fisher.eval.interfaces.Fieldiferous;
import fisher.runtime.ListTh;
import fisher.runtime.RecordTh;
import fisher.runtime.StringTh;
import fisher.runtime.Thing;
import fisher.runtime.ThingExtended;
import fisher.syn.core.Syntax;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  Elem extends XMLAbstract  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	// XML Element
	public StringTh tag;
	public RecordTh attrs;
	public ListTh children;

	public Elem(Thing tag, Thing attrs, Thing children) throws FisherException {
		super();
		try {
			this.tag = (StringTh) tag;
			this.attrs = attrs == null ? null : (RecordTh) attrs;
			this.children = children == null ? null
					: (children instanceof Elem || children instanceof StringTh) ? ListTh.of(children)
					: (children instanceof ListTh) ? (ListTh) children
					: ListTh.of(StringTh.of(EvalUtil.toString(children))); 
		} catch (ClassCastException cce) {
			Doom.runtime("XML(t,a,c) requires t:string, a:record, c:list/Elem/string", null, "t : "
					+ EvalUtil.kind(tag) + " = " + tag, "a : " + EvalUtil.kind(attrs) + " = "
					+ EvalUtil.toString(attrs), "c : " + EvalUtil.kind(children) + " = " + EvalUtil.toString(children));
		}
	}
	
	public Elem(Thing tag, Thing attrsOrChildren) throws FisherException {
		this(tag,
				attrsOrChildren instanceof RecordTh ? attrsOrChildren : null,
				attrsOrChildren instanceof ListTh   ? attrsOrChildren : null
				);
	}
	
	@Override
	void domulateUnder(Document doc, Node node) {
		Element el = doc.createElement(tag.toString());
		node.appendChild(el);
		if (attrs != null) {
			Map<String, Thing> fields = attrs.fields;
			for (Map.Entry<String, Thing> ent : fields.entrySet()) {
				String an = ent.getKey();
				String val = EvalUtil.toString(ent.getValue());
				el.setAttribute(an, val);
			}
		}
		if (children != null) {
			for (Thing child : children) {
				if (child == null)
					continue;
				else if (child instanceof XMLAbstract) {
					XMLAbstract xa = (XMLAbstract) child;
					xa.domulateUnder(doc, el);
				} else {
					String s = EvalUtil.toString(child);
					Text text = doc.createTextNode(s);
					el.appendChild(text);
				}
			}
		}
	}

	public String toString() {
		try {
			Document doc = this.domulate();
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			XMLFuns.serialize(doc, baos);
			String s = baos.toString();
			return s;
		} catch (Exception e) {
			e.printStackTrace();
			return "XML that wouldn't serialize because of " + e;
		}
	}

	public Thing kid(Thing kidTag) {
		String s = EvalUtil.toString(kidTag);
		Thing theChild = null;
		int nFound = 0;
		for (Thing child : children) {
			if (child instanceof Elem) {
				Elem childElem = (Elem) child;
				final StringTh childTag = childElem.tag.str();
				if (s.equals(childTag.toString())) {
					theChild = child;
					nFound += 1;
				}
			}
		}
		return (nFound == 1 ? theChild : null);
	}
	public Thing kids(Thing kidTag) {
		String s = EvalUtil.toString(kidTag);
		List<Thing> kids = new ArrayList<Thing>();
		for (Thing child : children) {
			if (child instanceof Elem) {
				Elem childElem = (Elem) child;
				final StringTh childTag = childElem.tag.str();
				if (s.equals(childTag.toString())) {
					kids.add(child);
				}
			}
		}
		return ListTh.fromJavaList(kids);
	}
	
	
	
	
	public Thing str(Thing ctrl) throws FisherException {
		Document doc = this.domulate();
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		Transformer serializer;
		try {
			serializer = XMLFuns.tfactory.newTransformer();
			//Setup indenting to "pretty print" -- defaults the same as str(), which is aka XMLFuns.serialize()
			serializer.setOutputProperty(OutputKeys.INDENT, XMLFuns.defaultIndent);
			serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, XMLFuns.omit);
			// Now, override from ctrl.
			if (ctrl instanceof RecordTh) {
				RecordTh rectrl = (RecordTh) ctrl;
				for (Map.Entry<String, Thing> fld : rectrl.fields.entrySet()) {
					String key = fld.getKey();
					final Thing value = fld.getValue();
					//					if (key.equals("indent-amount"))
					//						key = XMLFuns.INDENT_AMOUNT;
					serializer.setOutputProperty(key, value.toString());
				}
			}

			serializer.transform(new DOMSource(doc), new StreamResult(baos));

			String s = baos.toString();
			return StringTh.of(s);

		} catch (TransformerException e) {
			// this is fatal, just dump the stack and throw a runtime exception
			e.printStackTrace();

			throw new RuntimeException(e);
		}
	}

	public Thing tag() {
		return this.tag;
	}

	public Thing attrs() {
		return this.attrs;
	}

	public Thing children() {
		return this.children;
	}

	@Override
	public boolean equals(Object obj) {
		try {
			if (obj instanceof Elem) {
				Elem ox = (Elem) obj;
				return ox.tag.equals(this.tag) && EvalUtil.eq(this.attrs, ox.attrs)
						&& EvalUtil.eq(this.children, ox.children);
			} else
				return false;
		} catch (FisherException e) {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return tag.hashCode() ^ (attrs == null ? 0 : attrs.hashCode()) ^ (children == null ? 0 : children.hashCode());
	}

	//	public Thing getField(String fieldName, Syntax src) throws FisherException {
	//		if ("tag".equals(fieldName)) return this.tag();
	//		if ("attrs".equals(fieldName)) return this.attrs();
	//		if ("children".equals(fieldName)) return this.children();
	//		Doom.runtime("Not a field: " + fieldName, null);
	//		return null;
	//	}
	//	
	//	public boolean hasField(String fieldName) {
	//		return "tag".equals(fieldName) || "attrs".equals(fieldName) || "children".equals(fieldName);
	//	}
}
