
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.syn.converters;

import java.util.List;

import com.sun.java.swing.plaf.windows.WindowsTreeUI.ExpandedIcon;

import fisher.parser.ParseException;
import fisher.syn.BracketCall;
import fisher.syn.Cmd;
import fisher.syn.ExpExtract;
import fisher.syn.FieldRef;
import fisher.syn.FunCall;
import fisher.syn.MethodCall;
import fisher.syn.PostExp;
import fisher.syn.PostExpArgs;
import fisher.syn.PostExpBracketArgs;
import fisher.syn.PostExpDotId;
import fisher.syn.PostExpRecordArgs;
import fisher.syn.RecordCall;
import fisher.syn.VarExp;
import fisher.syn.core.Id;
import fisher.util.Bard;

public  class  PostfixesToExp  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  
	/* 
	 * This method handles the processing of expressions like 
	 *    a.x
	 *    a(1)
	 *    a.x(1)
	 *    a.x.y
	 *    a.x(1).y
	 *    a.x.y(1)
	 * The input is as a 'target' (e.g., 'a'), plus a list of PostExp's: 
	 *    a.x         -- [PostExpDotId("x")]
	 *    a(1)        -- [PostExpArgs(1)]
	 *    a.x(1)      -- [PostExpDotId("x"), PostExpArgs(1)]
	 * The desired output is a Cmd, most likely a MethodCall, FunCall, or FieldRef, which 
	 * captures the semantic intent of the expression.  This will, in general, produce a nested
	 * expression class: a.x(1).y will be a method call inside a field ref.
	 */

	public static Cmd convert(Cmd target, List<PostExp> posts, int at) throws ParseException{
		if (posts == null) return target;
		int n = posts.size();
		if (n == at) return target;
		if (n > at+1 && posts.get(at) instanceof PostExpDotId && posts.get(at+1) instanceof PostExpArgs) {
			PostExpDotId id = (PostExpDotId) posts.get(at);
			PostExpArgs args = (PostExpArgs) posts.get(at+1);
			Cmd nextTarget = new MethodCall(target.start,  args.end, target, id.id, args.args);
			return convert(nextTarget, posts, at+2);
		}
//		else if (n > at+1 && posts.get(at) instanceof PostExpDotId && posts.get(at+1) instanceof PostExpArgs) {
//			PostExpDotId id = (PostExpDotId) posts.get(at);
//			PostExpArrowed arr = (PostExpArrowed) posts.get(at+1);
//			Cmd nextTarget = new ExpExtract(target.start, arr.end, id.id, arr.args, arr.afters);
//			return convert(nextTarget, posts, at+2);
//		}
		PostExp post = posts.get(at);
		if (post instanceof PostExpDotId) {
			PostExpDotId id = (PostExpDotId) post;
			Cmd nextTarget = new FieldRef(target.start, post.end, target, id.id);
			return convert(nextTarget, posts, at+1);
		}
		else if (post instanceof PostExpArgs) {
			PostExpArgs args = (PostExpArgs) post;
			Cmd nextTarget = new FunCall(target.start, args.end, target, args.args);
			return convert(nextTarget, posts, at+1);
		}
		else if (post instanceof PostExpBracketArgs) {
			PostExpBracketArgs args = (PostExpBracketArgs) post;
			Cmd nextTarget = new BracketCall(target.start, args.end, target, args.args);
			return convert(nextTarget, posts, at+1);
		}
		else if (post instanceof PostExpRecordArgs) {
			PostExpRecordArgs rargs = (PostExpRecordArgs) post;
			Cmd nextTarget = new RecordCall(target.start, rargs.end, target, rargs.fields);	
			return convert(nextTarget, posts, at+1);
		}
		
		throw new ParseException(
				"Cannot understand expression \n" 
				+ " target = " + target + "\n" 
				+ " posts = " + Bard.sep(posts, ",  ") + "\n"
				+ " position = " + at + "\n"
				+ " post = " + post + "\n"
				+ " post details = " + post.details()
		);
		
	}
	
}
