
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fisher.eval.EvalUtil;
import fisher.eval.Evaller;
import fisher.eval.interfaces.Framelike;
import fisher.runtime.auxil.ImmutabilityContext;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.Doom;
import fisher.util.FisherException;

public  class  ListTh extends ThingImmutable implements Iterable<Thing>, Applyable  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public int length;
	Thing car;
	ListTh cdr;

	@Override
	public String typeString() {
		// TODO Auto-generated method stub
		return "list";
	}

	// null list constructor.
	private ListTh() {
		assert (EMPTY_HAS_BEEN_SET_DOOD == false);
		EMPTY_HAS_BEEN_SET_DOOD = true;
		this.length = 0;
		this.car = null;
		this.cdr = null;
	}

	private ListTh(Thing car, ListTh cdr) {
		this.car = car;
		this.cdr = cdr;
		this.length = cdr.length + 1;
	}

	public Thing car() {
		return this.car;
	}

	public ListTh cdr() {
		return this.cdr;
	}

	public int num() {
		return this.length;
	}
	
	@Override
	public Object unthingify() {
		// Convert lists into arrays, 'cause, um, it seems like a good idea.
		Object[] unthung = new Object[this.length];
		ListTh cursor = this;
		int i = 0;
		while (!cursor.isEmpty()) {
			unthung[i++] = cursor.car;
			cursor = cursor.cdr;
		}
		return unthung;
	}

	@Override
	public boolean canBeKey() {
		return EvalUtil.canBeKey(car) && EvalUtil.canBeKey(cdr);
	}

	public static ListTh of(Thing... things) throws FisherException {
		ListTh L = EMPTY;
		for (int i = things.length - 1; i >= 0; i--) {
			L = L.cons(things[i]);
		}
		return L;
	}
	
	public Thing[] toJavaArray() {
		Thing[] res = new Thing[this.length];
		int i = 0;
		for(Thing v : this) {
			res[i++] = v;
		}
		return res;
	}

	@Override
	public boolean isImmutable(ImmutabilityContext ic) {
		if (ic != null)
			ic.seen(this);
		for (Thing thing : this) {
			if (!EvalUtil.isImmutable(thing, ic))
				return false;
		}
		return true;
	}

	public ListTh cons(Thing th) {
		return new ListTh(th, this);
	}

	public ListTh appendBehind(ListTh onFront) {
		if (onFront.isEmpty())
			return this;
		// onFront = [a, b...]; this = [c...]
		ListTh bc = this.appendBehind(onFront.cdr()); // bc = [b..., c...]
		ListTh abc = bc.cons(onFront.car);
		return abc;
	}

	public ListTh followedBy(Thing atEnd) throws FisherException {
		ListTh L = ListTh.of(atEnd);
		L = consOnto(L, this);
		return L;
	}

	public ListTh reversed() {
		return reverseOnto(this, EMPTY);
	}

	private static ListTh consOnto(ListTh laterStuff, ListTh earlierStuff) throws FisherException {
		if (earlierStuff == EMPTY)
			return laterStuff;
		ListTh L = consOnto(laterStuff, earlierStuff.cdr());
		L = L.cons(earlierStuff.car());
		return L;
	}

	private static ListTh reverseOnto(ListTh from, ListTh to) {
		if (from.isEmpty())
			return to;
		ListTh R = reverseOnto(from.cdr(), to.cons(from.car()));
		return R;
	}

	public final boolean isEmpty() {
		return this.length == 0;
	}

	public final boolean notEmpty() {
		return !this.isEmpty();
	}

	private static boolean EMPTY_HAS_BEEN_SET_DOOD = false;
	public static final ListTh EMPTY = new ListTh();

	@Override
	public Iterable<Thing> asIter(Syntax src) throws FisherException {
		return this;
	}

	public Iterator<Thing> iterator() {
		Iterator<Thing> iter = new Iterator<Thing>() {
			ListTh cursor = ListTh.this;

			public boolean hasNext() {
				return !cursor.isEmpty();
			}

			public Thing next() {
				Thing T = cursor.car;
				cursor = cursor.cdr;
				return T;
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
		return iter;
	}

	public Thing sub(long l, Syntax src) throws FisherException {
		int i = unflipChecked(l, src);
		for (Thing thing : this) {
			if (i == 0)
				return thing;
			i -= 1;
		}
		Doom.runtime("Subscript out of range: " + l + " needs to be in the range -" + length + " .. " + (length - 1),
				src, this, l, i, unflipChecked(l, src));
		return null;
	}

	/**
	 * @param n
	 * @param src
	 * @return the nth cdr of this. <b>Warning:</b> it is not very friendly of
	 *         error message. Best to check n <= L.num() first.
	 * @throws FisherException
	 */
	public ListTh nthCdr(long n, Syntax src) throws FisherException {
		if (n == 0)
			return this;
		if (this.isEmpty() && n > 0) {
			Doom.runtime("nthCdr out of bounds", src);
		}
		return this.cdr().nthCdr(n - 1, src);
	}

	/**
	 * @param n
	 * @param src
	 * @return the prefix of size n of this.
	 * @throws FisherException
	 */
	public ListTh prefix(int n, Syntax src) throws FisherException {
		if (n == this.num())
			return this;
		if (n > this.num()) {
			Doom.runtime("Can't take prefix of length " + n + " of a list of length " + this.num(), src, this, n);
		}
		if (n < 0) {
			Doom.runtime("Negative length? " + n, src, this, n);
		}
		return prefixInternal(n);
	}

	private ListTh prefixInternal(int n) {
		if (n == 0)
			return EMPTY;
		else {
			ListTh preOfCdr = this.cdr().prefixInternal(n - 1);
			ListTh prefix = preOfCdr.cons(this.car);
			return prefix;
		}
	}

	public ListTh sublist(long from, long to, Syntax src) throws FisherException {
		int i = (int) from;
		int j = (int) to;
		if (j < i)
			return EMPTY;
		int size = j - i ;
		ListTh I = this.nthCdr(i, src);
		ListTh L = I.prefix(Math.min(size, I.length), src);
		return L;
	}

	public int unflipUnchecked(long l) throws FisherException {
		int j = (int) (l >= 0 ? l : length + l);
		if (j < 0) j = 0;
		if (j > length) j = length;
		return j;
	}
	public int unflipChecked(long l, Syntax src) throws FisherException {
		int j = (int) (l >= 0 ? l : length + l);
		if (j < 0 || j > length) {
			Doom.runtime("Subscript out of bounds: " + l + " for a list of length " + length, src, this, j);
		}
		return j;
	}

	public Thing apply(Thing[] args, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		switch (args.length) {
		case 1:
			return apply1(args[0], frame, evaller, src);
//		case 2:
//			return apply2(args[0], args[1], frame, evaller, src);
		default: {
			Doom.runtime("List can't be subscripted by (" + Bard.sep(args, ", ") + ")", src, args, frame, evaller, src);
			return null;
		}
		}
	}

	public Thing apply1(Thing arg0, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		if (arg0 == null) {
			Doom.runtime("Can't subscript a list by null! arg0 = " + arg0, src, this, arg0, frame, evaller);
			return null;
		} else if (arg0.isLong()) {
			return this.sub(arg0.asLong(src), src);
		} else if (arg0.isIntRange()) {
			IntRangeTh range = arg0.asIntRange(src);
			if (range.min >= 0 && range.max >= 0) {
				return this.sublist(range.min, range.max, src);
			} else {
				Doom.runtime("Can't subscript a list by " + range, src, this, range);
				return null;
			}
		} else {
			Doom.runtime("Can't subscript a list by " + arg0.typeString() + " (= " + arg0 + ")", src, this, arg0);
			return null;
		}
	}

	public Thing apply2(Thing arg0, Thing arg1, Framelike frame, Evaller evaller, Syntax src) throws FisherException {
		if (arg0 == null || arg1 == null) {
			Doom.runtime("Can't subscript a list by null! arg0 = " + arg0 + ", arg1 = " + arg1, src, this, arg0, arg1,
					frame, evaller);
			return null;
		} else if (arg0.isLong() && arg1.isLong()) {
			return this.sublist((int) arg0.asLong(src), (int) arg1.asLong(src), src);
		} else {
			Doom.runtime("Can't subscript a list by " + arg0.typeString() + "," + arg1.typeString() + " (= " + arg0
					+ "," + arg1 + ")", src, this, arg0);
			return null;
		}
	}

	@Override
	public boolean equals(Object other) {
		if (other.getClass() != ListTh.class)
			return false;
		ListTh lother = (ListTh) other;
		if (this.length != lother.length)
			return false;
		Iterator<Thing> itThis = this.iterator();
		Iterator<Thing> itOther = lother.iterator();
		while (itThis.hasNext() && itOther.hasNext()) {
			Thing xThis = itThis.next();
			try {
				Thing xOther = itOther.next();
				if (!EvalUtil.eq(xThis, xOther))
					return false;
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		return true;
	}

	@Override
	public int hashCode() {
		int hc = 0;
		for (Thing thing : this) {
			hc ^= thing == null ? 12321 : thing.hashCode();
		}
		return hc;
	}

	@Override
	public boolean isList() {
		return true;
	}

	public ListTh asList(Syntax src) throws FisherException {
		return this;
	}

	public String toString() {
		return "[" + Bard.sep(this, ", ") + "]";
	}

	public List<Thing> toJavaList() {
		List<Thing> jl = new ArrayList<Thing>(this.length);
		for (Thing thing : this) {
			jl.add(thing);
		}
		return jl;
	}

	public static ListTh fromJavaList(List<? extends Thing> things) {
		ListTh L = EMPTY;
		for (int i = things.size() - 1; i >= 0; i--) {
			L = L.cons(things.get(i));
		}
		return L;
	}

	/**
	 * @param things
	 * @return the things as a Thorn list, in no particular order. Well -- if
	 *         the collection is ordered, the ListTh will be backwards.
	 */
	public static ListTh fromJavaCollection(Collection<? extends Thing> things) {
		ListTh L = EMPTY;
		for (Thing thing : things) {
			L = L.cons(thing);
		}
		return L;
	}

	public ListTh flatten() {
		List<Thing> L = new ArrayList<Thing>();
		flattenInto(L);
		return fromJavaList(L);
	}

	private void flattenInto(List<Thing> jl) {
		for (Thing thing : this) {
			if (thing instanceof ListTh) {
				ListTh list = (ListTh) thing;
				list.flattenInto(jl);
			} else {
				jl.add(thing);
			}
		}
	}

	private boolean subsetEq(ListTh other) throws FisherException {
		for (Thing thing : this) {
			if (!other.contains(thing))
				return false;
		}
		return true;
	}

	private boolean contains(Thing thing) throws FisherException {
		for (Thing mything : this) {
			if (EvalUtil.eq(mything, thing))
				return true;
		}
		return false;
	}

	private final static int STR = 0;
	private final static int REVERSED = 1;
	private final static int LEN = 2;
	private final static int RANGE = 3;
	private final static int Crand = 5;
	private final static int Cunique = 6;
	private final static int CsubsetEq = 7;
	private final static int CsetEq = 8;
	private final static int Cunion = 9;
	private final static int Cintersect = 10;
	private final static int Cminus = 11;
	private final static int ChashCode = 12;
	private final static int Cspan = 13;
	private final static int Czip = 14;
	private final static int Cindex = 15;
	private final static int CfollowedBy = 16;
	private final static int Cjoined = 17;
	private final static int Cand = 18;
	private final static int Ctails = 19;
	private final static int Ctail = 20;
	private final static int Chead = 21;
	private final static int Cflat1 = 22;
	private final static int Cflat = 23;
	private final static int Cinterleave = 24;
	private final static int Cnumbered = 25;
	private final static int CasBytes = 26;
	private static final int Csublen = 27;
	private static final int Csub = 28;
	private static final int Csubx = 29;
	private static final int Cleft = 30;
	private static final int Cbutleft = 31;
	private static final int Cright = 32;
	private static final int Cbutright = 33;
	private static final int Crange = 34;
	private static final int Csum = 35;
	private static final int Ccat = 36;

	private static Map<String, Integer> methodCode = new HashMap<String, Integer>();
	static {
		methodCode.put("reversed", REVERSED);
		methodCode.put("reverse", REVERSED);
		methodCode.put("len", LEN);
		methodCode.put("num", LEN);
		methodCode.put("str", STR);
		methodCode.put("range", RANGE);
		methodCode.put("rand", Crand);
		methodCode.put("unique", Cunique);
		methodCode.put("subset", CsubsetEq);
		methodCode.put("subset?", CsubsetEq);
		methodCode.put("setEq", CsetEq);
		methodCode.put("setEq?", CsetEq);
		methodCode.put("union", Cunion);
		methodCode.put("intersect", Cintersect);
		methodCode.put("minus", Cminus);
		methodCode.put("hashCode", ChashCode);
		methodCode.put("span", Cspan);
		methodCode.put("zip", Czip);
		methodCode.put("index", Cindex);
		methodCode.put("followedBy", CfollowedBy);
		methodCode.put("joined", Cjoined);
		methodCode.put("and", Cand);
		methodCode.put("tails", Ctails);
		methodCode.put("tail", Ctail);
		methodCode.put("head", Chead);
		methodCode.put("flat1", Cflat1);
		methodCode.put("flat", Cflat);
		methodCode.put("interleave", Cinterleave);
		methodCode.put("numbered", Cnumbered);
		methodCode.put("asBytes", CasBytes);
		methodCode.put("sub", Csub);
		methodCode.put("subx", Csubx);
		methodCode.put("left", Cleft);
		methodCode.put("butleft", Cbutleft);
		methodCode.put("right", Cright);
		methodCode.put("butright", Cbutright);
		methodCode.put("sublen", Csublen);
		methodCode.put("range", Crange);
		methodCode.put("sum", Csum);
		methodCode.put("cat", Ccat);
	}

	@Override
	public Thing invokeMethod(String methodName, Thing[] args,   Syntax src)
			throws FisherException {
		if (methodCode.containsKey(methodName)) {
			Integer methodC = methodCode.get(methodName);
			Evaller evaller = Evaller.mine();
			Framelike frame = this.frameForInvoke();
			switch (methodC) {
			case STR:
				return this.str();
			case REVERSED:
				return this.reversed();
			case LEN:
				return this.Mlen();
			case RANGE:
				return this.Mrange();
			case Crand:
				return this.Crand(methodName, args, evaller, frame, src);
			case Cunique:
				return this.Cunique(methodName, args, evaller, frame, src);
			case CsubsetEq:
				return this.CsubsetEq(methodName, args, evaller, frame, src);
			case CsetEq:
				return this.CsetEq(methodName, args, evaller, frame, src);
			case Cunion:
				return this.Cunion(methodName, args, evaller, frame, src);
			case Cintersect:
				return this.Cintersect(methodName, args, evaller, frame, src);
			case Cminus:
				return this.Cminus(methodName, args, evaller, frame, src);
			case ChashCode:
				return this.ChashCode(methodName, args, evaller, frame, src);
			case Cspan:
				return this.Cspan(methodName, args, evaller, frame, src);
			case Czip:
				return this.Czip(methodName, args, evaller, frame, src);
			case Cindex:
				return this.Cindex(methodName, args, evaller, frame, src);
			case CfollowedBy:
				return this.CfollowedBy(methodName, args, evaller, frame, src);
			case Cjoined:
				return this.Cjoined(methodName, args, evaller, frame, src);
			case Cand:
				return this.Cand(methodName, args, evaller, frame, src);
			case Ctails:
				return this.Ctails(methodName, args, evaller, frame, src);
			case Ctail:
				return this.Ctail(methodName, args, evaller, frame, src);
			case Chead:
				return this.Chead(methodName, args, evaller, frame, src);
			case Cflat1:
				return this.Cflat1(methodName, args, evaller, frame, src);
			case Cflat:
				return this.Cflat(methodName, args, evaller, frame, src);
			case Cinterleave:
				return this.Cinterleave(methodName, args, evaller, frame, src);
			case Cnumbered:
				return this.Cnumbered(methodName, args, evaller, frame, src);
			case CasBytes:
				return this.CasBytes(methodName, args, evaller, frame, src);
			case Csub:
				return this.Csub(methodName, args, evaller, frame, src);
			case Csubx:
				return this.Csubx(methodName, args, evaller, frame, src);
			case Cleft:
				return this.Cleft(methodName, args, evaller, frame, src);
			case Cbutleft:
				return this.Cbutleft(methodName, args, evaller, frame, src);
			case Cright:
				return this.Cright(methodName, args, evaller, frame, src);
			case Cbutright:
				return this.Cbutright(methodName, args, evaller, frame, src);
			case Csublen:
				return this.Csublen(methodName, args, evaller, frame, src);
			case Crange:
				return this.Crange(methodName, args, evaller, frame, src);
			case Csum:
				return this.Csum(methodName, args, evaller, frame, src);
			case Ccat:
				return this.Ccat(methodName, args, evaller, frame, src);
			default:
				Doom.internal("Method key structure broken", src, methodC);
				return null;
			}
		}
		return this.tryThingMethods(methodName, args,  src);
	}

	private Thing Ccat(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (this.length == 0) return StringTh.EMPTY_STRING;
		StringBuffer sb = new StringBuffer();
		for (Thing thing : this) {
			sb.append(EvalUtil.toString(thing));
		}
		return StringTh.of(sb.toString());
	}
	
	private Thing Csum(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (this.length == 0) return IntTh.of(0);
		if (this.length == 1) return this.car;
		Thing sum = this.car;
		for (Thing thing : this.cdr) {
			if (sum == null) return null;
			sum = sum.invokeMethod("+", Bard.array(thing),  src);
		}
		return sum;
	}
	
	private Thing Crange(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return IntRangeTh.of(0, this.length-1);
	}
	private Thing Csublen(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int startMaybeNeg = (int) args[0].asLong(src);
		int start = unflipChecked(startMaybeNeg, src);
		int length = (int) args[1].asLong(src);
		int end = start + length;
		if (end > length)
			end = length+1;
		Thing ss = this.sublist(start, end, src);
		return ss;
	}

	private Thing Cbutright(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take the last " + i + " elements of a list.", src);
		final int n = this.length;
		if (i > n)
			i = n;
		Thing th = sublist(0, n - i,src);
		return th;
	}

	private Thing Cright(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take the last " + i + " elements of a list.", src);
		final int n = this.length;
		if (i > n)
			i = n;
		Thing t = sublist(n - i, n, src);
		return t;
	}

	private Thing Cbutleft(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take everything but the first " + i + " elements of a list.", src);
		if (i > length) i = length;
		Thing t = sublist(i, this.length, src);
		return t;
	}

	private Thing Cleft(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		if (i < 0)
			Doom.runtime("Can't take the first " + i + " elements of a list.", src);
		if (i > length) i = length;
		Thing t = sublist(0,i,src);
		return t;
	}

	private Thing Csubx(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi =  unflipChecked(i, src);
		int j = args[1].asJavaInt(src);
		int pj = unflipChecked(j, src);
		if (pj > length) pj = length;
		Thing t = sublist(pi, pj,src);
		return t;
	}

	private Thing Csub(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		int i = args[0].asJavaInt(src);
		int pi = unflipChecked(i, src);
		int j = args[1].asJavaInt(src);
		int pj = unflipChecked(j, src);
		if (pj > length) pj = length;
		Thing t = sublist(pi, pj + 1, src);
		return t;
	}

	private Thing CasBytes(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		int n = this.length;
		byte[] bytes = new byte[n];
		int k = 0;
		for (Thing t : this) {
			int i = t.asJavaInt(src);
			if (i < Byte.MIN_VALUE || i > Byte.MAX_VALUE) {
				Doom.runtime("Not a byte value: " + i, src);
			}
			bytes[k++] = (byte) i;
		}
		BytesTh bytesth = new BytesTh(bytes);
		return bytesth;
	}

	private Thing Cinterleave(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		ListTh L = args[0].asList(src);
		if (L.length != this.length && L.length != this.length - 1) {
			Doom.runtime("Can't interleave a list of length " + this.length + " with one of length " + L.length, src,
					"this=" + this, "L=" + L);
			return null;
		} else {
			List<Thing> IL = new ArrayList<Thing>(this.length + L.length);
			Iterator<Thing> thisit = this.iterator();
			Iterator<Thing> Lit = L.iterator();
			while (thisit.hasNext()) {
				IL.add(thisit.next());
				if (Lit.hasNext())
					IL.add(Lit.next());
			}
			return ListTh.fromJavaList(IL);
		}
	}

	private Thing CfollowedBy(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing target = args[0];
		return this.followedBy(target);
	}

	private Thing Cjoined(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		String separator = args[0].asString(src);
		;
		String res = Bard.sep(this, separator);
		return StringTh.of(res);
	}

	private static String[] KV = Bard.array("k", "v");

	private Thing Cnumbered(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		List<Thing> recs = new ArrayList<Thing>(this.length);
		int i = 0;
		for (Thing thing : this) {
			RecordTh rec = RecordTh.make(src, KV, IntTh.of(i), thing);
			recs.add(rec);
			i += 1;
		}
		return ListTh.fromJavaList(recs);
	}

	private Thing Ctail(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (this.isEmpty())
			return null;
		else
			return this.cdr;
	}

	private Thing Chead(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (this.isEmpty()) {
			Doom.runtime("Can't take head of empty list", src);
			return null;
		} else
			return this.car();
	}

	private Thing Cflat1(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		List<Thing> jl = new ArrayList<Thing>();
		for (Thing thing : this) {
			if (thing.isList()) {
				//				ListTh L2 = thing.asList(src);
				for (Thing thing2 : EvalUtil.iter(thing, src)) {
					jl.add(thing2);
				}
			} else {
				jl.add(thing);
			}
		}
		return ListTh.fromJavaList(jl);
	}

	private Thing Cflat(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		List<Thing> jl = new ArrayList<Thing>();
		flatTo(this, jl, src);
		return ListTh.fromJavaList(jl);
	}

	private static void flatTo(Thing L, List<Thing> jl, Syntax src) throws FisherException {
		if (L.isList()) {
			for (Thing thing : L.asList(src)) {
				flatTo(thing, jl, src);
			}
		} else {
			jl.add(L);
		}
	}

	private Thing Ctails(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 1, methodName, args, evaller, frame, src);
		int minLen = args.length == 0 ? 0 : args[0].asJavaInt(src);
		return Ctails(this, minLen);
	}

	private ListTh Ctails(ListTh t, int minLen) throws FisherException {
		if (t.length < minLen)
			return EMPTY;
		if (t == EMPTY) {
			return ListTh.of(EMPTY);
		} else {
			ListTh later = Ctails(t.cdr(), minLen);
			return later.cons(t);
		}
	}

	private Thing Cand(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		switch (this.length) {
		case 0:
			return StringTh.EMPTY_STRING;
		case 1:
			return StringTh.of(EvalUtil.toString(this.car()));
		case 2:
			return StringTh.of(EvalUtil.toString(this.car()) + " and " + EvalUtil.toString(this.cdr().car()));
		default:
			StringBuffer sb = new StringBuffer();
			int i = 1;
			for (Thing thing : this) {
				sb.append(EvalUtil.toString(thing));
				if (i < length - 1) {
					sb.append(", ");
				} else if (i == length - 1) {
					sb.append(", and ");
				} else if (i == length) {
					// Nothing
				} else {
					Doom.internalCatastrophe("Math is broken!", src, i, this.length, this);
				}
				i++;
			}
			return StringTh.of(sb.toString());
		}
	}

	private Thing Cindex(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing target = args[0];
		long i = 0;
		ListTh cursor = this;
		while (cursor != ListTh.EMPTY && !(EvalUtil.eq(target, cursor.car))) {
			cursor = cursor.cdr;
			i += 1;
		}
		if (cursor == ListTh.EMPTY)
			return null;
		else
			return IntTh.of(i);
	}

	private ListTh Czip(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		Thing l = args[0];
		List<Thing> things = Bard.list();
		// TODO: This is O(n^2) for the common case that l is a List.  
		if (l instanceof Applyable) {
			Applyable apl = (Applyable) l;
			ListTh cursor = this;
			for (int i = 0; i < this.length; i++) {
				Thing a = cursor.car;
				cursor = cursor.cdr;
				IntTh ith = IntTh.of(i);
				Thing b = apl.apply(Bard.array((Thing) ith), frame, evaller, src);
				things.add(ListTh.of(a, b));
			}
		} else {
			Doom.runtime("Can't zip with a " + l.typeString(), src, "this: ", this, "l: ", l);
		}
		return fromJavaList(things);
	}

	private ListTh Cspan(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(2, 2, methodName, args, evaller, frame, src);
		long start = args[0].asLong(src);
		long len = args[1].asLong(src);
		if (len <= 0)
			return EMPTY;
		long max = start + len - 1;
		if (max > this.length - 1)
			max = this.length - 1;
		if (max < start)
			return EMPTY; // Possible for [].span(0,2);
		return sublist(start, max, src);
	}

	private Thing Cminus(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		ListTh other = args[0].asList(src);
		ListTh L = EMPTY;
		for (Thing thing : this) {
			if (!other.contains(thing)) {
				L = L.cons(thing);
			}
		}
		return L.reversed();
	}

	private Thing Cintersect(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		ListTh other = args[0].asList(src);
		ListTh L = EMPTY;
		for (Thing thing : this) {
			if (other.contains(thing)) {
				L = L.cons(thing);
			}
		}
		return L;
	}

	private Thing CsetEq(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		ListTh other = args[0].asList(src);
		return BoolTh.of(this.subsetEq(other) && other.subsetEq(this));
	}

	private Thing Cunion(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		ListTh other = args[0].asList(src);
		ListTh both = this.appendBehind(other);
		return both.unique();
	}

	private Thing CsubsetEq(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(1, 1, methodName, args, evaller, frame, src);
		return BoolTh.of(this.subsetEq(args[0].asList(src)));
	}

	private Thing Crand(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		if (length == 0)
			Doom.runtime("Can't take a random element from the empty list.", src);
		int r = Bard.random.nextInt(length);
		return this.sub(r, src);
	}

	private Thing Cunique(String methodName, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
			throws FisherException {
		checkNumberOfArgs(0, 0, methodName, args, evaller, frame, src);
		return this.unique();
	}

	private Thing unique() {
		List<Thing> L = this.toJavaList();
		Set<Thing> S = new HashSet<Thing>(L);
		List<Thing> LU = new ArrayList<Thing>(S);
		return ListTh.fromJavaList(LU);
	}

	private IntTh Mlen() {
		return IntTh.of(this.length);
	}

	private IntRangeTh Mrange() throws FisherException {
		return IntRangeTh.of(0, this.length - 1);
	}

	//	public Internal_Success invokePat(Id patId, Thing[] args, Evaller evaller, Framelike frame, Syntax src)
	//	throws FisherException {
	//		return this.patNotUnderstood(patId, args, evaller, frame, src);
	//	}

}
