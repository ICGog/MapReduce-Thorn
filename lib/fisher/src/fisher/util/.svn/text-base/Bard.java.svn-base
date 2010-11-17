
/** 
 * Copyright (c) 2009 IBM Corp. 
 * 
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html. If redistributing this code, 
 * this entire header must remain intact. 
 */ 
package fisher.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.*;

public  class  Bard  { static String copyright() { return fisher.util.Copyright.IBM_COPYRIGHT; }
  

	public static final String MY_FAVORITE_ENCODING = "UTF-8";
	public static Random random = new Random();
	public static int rand(int low, int high) {
		int d = high - low + 1;
		return low + random.nextInt(d);
	}

	public static String sep(final int[] words, final String separator) {
		StringBuffer sb = new StringBuffer();
		if (words == null)
			return "";
		for (int i = 0; i < words.length; ++i) {
			sb.append(words[i] + ""); //$NON-NLS-1$ 
			if (i < words.length - 1) {
				sb.append(separator);
			}
		}
		return sb.toString();
	}

	public static String sep(final Object[] words, final String separator) {
		StringBuffer sb = new StringBuffer();
		if (words == null)
			return "";
		for (int i = 0; i < words.length; ++i) {
			sb.append(words[i] + ""); //$NON-NLS-1$ 
			if (i < words.length - 1) {
				sb.append(separator);
			}
		}
		return sb.toString();
	}
	
	public static <K,V> String sep2(Set<Map.Entry<K, V>> stuff, String betweenKandV, String betweenEntries) {
		StringBuffer sb = new StringBuffer();

		for (Iterator<Map.Entry<K, V>> iter = stuff.iterator(); iter.hasNext();) {
			Map.Entry<K, V> entry = iter.next();
			sb.append(entry.getKey() + betweenKandV + entry.getValue() + ""); //$NON-NLS-1$ 
			if (iter.hasNext()) {
				sb.append(betweenEntries);
			}
		}
		return sb.toString();
	}

	public static <T> T last(List<T> list) {
		return list.get(list.size() - 1);
	}
	
	public static <T> List<T> sublist(List<T> L, int from, int to) {
		List<T> SL = list();
		for(int i = from; i <= to; i++) {
			SL.add(L.get(i));
		}
		return SL;
	}

	public static String sep(Iterable c, String separator) {
		StringBuffer sb = new StringBuffer();

		for (Iterator iter = c.iterator(); iter.hasNext();) {
			Object element = iter.next();
			sb.append(element + ""); //$NON-NLS-1$ 
			if (iter.hasNext()) {
				sb.append(separator);
			}
		}
		return sb.toString();
	}

	public static <T> List<T> list(T... x) {
		List<T> L = new ArrayList<T>(x.length);
		Collections.addAll(L, x);
		return L;
	}

	public static <T> Set<T> set(T... x) {
		Set<T> S = new HashSet<T>(x.length);
		Collections.addAll(S, x);
		return S;
	}

	public static <T> List<T> array2list(T[] a) {
		List<T> L = new ArrayList<T>(a.length);
		Collections.addAll(L, a);
		return L;
	}

	public static Object[] array_obj(Object... w) {
		return w;
	}

	public static <T> T[] array(T... w) {
		return w;
	}

	public static <T> List<T> reverse(List<T> L) {
		List<T> R = new ArrayList<T>(L.size());
		for (T n : L) {
			R.add(0, n);
		}
		return R;
	}

	public static <T> Set<T> union(Set<T>... sets) {
		Set<T> U = new HashSet<T>();
		for (Set<T> s : sets) {
			U.addAll(s);
		}
		return U;
	}

	public static <T> T someElementOf(Collection<T> stuff) {
		for (T t : stuff) {
			return t;
		}
		return null;
	}

	public static <T> List<T> cat(Collection<T>... lists) {
		// May as well get the size right
		int size = 0;
		for (Collection<T> collection : lists) {
			size += collection.size();
		}
		List<T> L = new ArrayList<T>(size);
		for (Collection<T> collection : lists) {
			L.addAll(collection);
		}
		return L;
	}

	public static <T> Set<T> minus(Set<T> from, Set<T>... removes) {
		Set<T> M = new HashSet<T>();
		M.addAll(from);
		for (Set<T> remove : removes) {
			M.removeAll(remove);
		}
		return M;
	}

	public static <T> boolean subset(Set<T> smaller, Set<T> larger) {
		for (T t : smaller) {
			if (!(larger.contains(t)))
				return false;
		}
		return true;
	}

	public static <T> boolean bagEq(Collection<T> a, Collection<T> b) {
		Map<T, Integer> counts = new HashMap<T, Integer>();
		for (T ta : a) {
			if (counts.containsKey(ta)) {
				counts.put(ta, counts.get(ta) + 1);
			} else {
				counts.put(ta, 1);
			}
		}
		// counts now = counts for a.
		for (T tb : b) {
			if (counts.containsKey(tb)) {
				counts.put(tb, counts.get(tb) - 1);
			} else {
				// No instances of tb in a!
				return false;
			}
		}
		// counts now = #a - #b.  Should all be zero.
		for (Integer n : counts.values()) {
			if (n != 0)
				return false;
		}
		return true;
	}

	public static String str(Object o) {
		if (o == null)
			return "null";
		if (o instanceof Collection)
			return "[" + sep((Collection) o, ",") + "]";
		return o.toString();
	}

	/**
	 * @param <T>
	 * @param ts
	 * @return the first of the arguments which is not null, or null if all are.
	 *         Note that this evaluates all arguments!
	 */
	public static <T> T firstNonNull(T... ts) {
		for (T t : ts) {
			if (t != null)
				return t;
		}
		return null;
	}
	
	
	public static void printNonASCIIChars(String s) {
		for(int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (c > '~') {
				System.err.println("printNonASCII: (" + i + ")=" + c);
			}
		}
	}
	
	public static void setContents(File file, String newContents)  {
		try {
			FileOutputStream fos = new FileOutputStream(file, false);
			Writer wr = (new OutputStreamWriter(fos, Bard.MY_FAVORITE_ENCODING));
			wr.write(newContents);
			wr.flush();
			wr.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * @param f
	 * @return approximately the contents of file f. It might append a newline
	 *         to the last line.
	 */
	public static String contentsOf(File f) {
		
		String s =  contentsOf(f, Bard.MY_FAVORITE_ENCODING);
//		System.err.println("contentsOf(" + f + ") = \n" + s);
//		printNonASCIIChars(s);
		return s;
	}
	public static String contentsOf(File f, String encoding) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(f), encoding));
			StringBuffer sb = new StringBuffer();
			while (in.ready()) {
				sb.append(in.readLine());
				sb.append("\n");
			}
			return sb.toString();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return null;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	public static String contentsOf(String s) {
		return contentsOf(new File(s));
	}

	/**
	 * @param s
	 * @param from
	 * @param to
	 * @return a substring of s from from to to, but stopping at the ends of the
	 *         string even if from or to is out of bounds.
	 */
	public static String substring(String s, int from, int to) {
		return s.substring(Math.max(0, from), Math.min(to, s.length()));
	}

	public static long longPower(long base, long exponent) {
		if (exponent < 0)
			throw new RuntimeException("Integer powers please.");
		if (exponent == 0)
			return 1;
		if (exponent == 1)
			return base;
		long a = exponent >> 1;
		long b = exponent & 1;
		long p = longPower(base, a);
		return (b == 1) ? base * p * p : p * p;
	}

	public static String shortClassName(Object o) {
		String longname = o.getClass().toString();
		int dot = longname.lastIndexOf(".");
		return longname.substring(dot + 1);
	}

	public static byte[] serialization(Object whatever) {
		try {
			ByteArrayOutputStream outputStream = new ByteArrayOutputStream(); 
			ObjectOutputStream oos = new ObjectOutputStream(outputStream);
			oos.writeObject(whatever);
			oos.close();
            String s;
            try {
			    //s = outputStream.toString("UCS-4");
            	byte[] bytes = outputStream.toByteArray();
            	return bytes;
            } catch (Exception e){
            	return null;
            }

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}		
	}
	
	public static Object deserialization(byte[] bytes) {
		try {
            ByteArrayInputStream bais;
            try {
            	bais = new ByteArrayInputStream(bytes);
//			    bais = new ByteArrayInputStream(ser.getBytes("UCS-4"));
            } catch (Exception e) { return null;}
			ObjectInputStream ois = new ObjectInputStream(bais);
			return ois.readObject();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * @param s
	 * @return a string from which s can be decoded easily.  I hope.
	 */
	public static String serStr(String s) {
		return "serStr[" + s.length() + ":" + s + "]";
	}

}
