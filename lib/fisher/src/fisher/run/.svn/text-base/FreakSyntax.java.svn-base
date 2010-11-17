package fisher.run;

import java.io.File;

import fisher.parser.FisherParser;
import fisher.parser.SyntacticClass;
import fisher.syn.core.Syntax;
import fisher.util.Bard;
import fisher.util.FisherSource;

public class FreakSyntax {
	
	public static boolean convert = false;
	
	static String orig = "/Users/bard/thorn/fisherws/fisher/testcase";
	static String nova = "/Users/bard/thorn/tmp/nova";
	
	public static void main(String[] crap) {
		System.out.println("Converting files from old Thorn to new Thorn.");
		System.out.println("orig = " + orig);
		System.out.println("nova = " + nova);
		freak( new File(orig), new File(nova)); 	
		
	}
	
	static void freak(File ori, File nov) {
//		System.out.println("Freaking " + ori + " -> " + "nov");
		if (ori.getName().endsWith(".th")) freakThorn(ori, nov);
		else if (ori.getName().endsWith(".thm")) freakThorn(ori, nov);
		else if (ori.getName().contains("svn")) return;
		else if (ori.isDirectory()) freakDir(ori, nov);
		else System.out.println("Ignoring " + ori + " ~ " + nov);
	}
	
	static void freakThorn(File ori, File nov) {
//		System.err.println("Thorn file " + ori + " >>> " + nov );
		FisherSource src = new FisherSource.FromFile(ori);
		FisherParser parser = src.parser();
		final Syntax syn = SyntacticClass.parseAnyhowPossible(parser, src);
		if (syn != null) {
			Bard.setContents(nov, syn.toString());
		}
	}

	static void freakDir(File ori, File nov) {
		System.out.println("Woot, directory!");
		if (!nov.exists()) {
			nov.mkdirs();
		}
		final File[] oriFiles = ori.listFiles();
		for (File orich : oriFiles ){
			File novch = new File(nov, orich.getName());
			freak(orich, novch);
		}
		System.out.println("Done with directory " + ori + " >>> " + nov);
	}
	
}
