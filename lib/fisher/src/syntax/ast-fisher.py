import os.path


WARNING = "/* Beware!  This is a generated file.  Change the generator's dataset! */"

Globals = {}

class Gene:
    def __init__(this, rootDir, package): 
        this.rootDir = rootDir
        this.package = package
        this.visitorPackage = package + ["visitor"]
        print "visitorPackage = %s" % this.visitorPackage
    def __str__(this):
        return "Gene(rootDir=%s, package=%s)" % (this.rootDir, this.package)
    def filename(this, c, suffix=".java", package=None):
        a = this.rootDir
        if package == None:
            package = this.package
        b = "/".join(package)
        s = "%s/%s/%s%s" % (a,b,c,suffix)
        return s
    def thepackage(this):
        return ".".join(this.package)
    def visitorLines(this, rootclass = None, ret="RET"):
        if rootclass == None:
            rootclass = Globals["Syntax"]
        return [ "    %s visit(%s syn, ARG arg) throws EXN;" % (ret, cl.jname())
            for cl in AllSyntaxClasses
            if cl.extends(rootclass)
            ]
    def visitorLines2(this, rootclass = None, ret="RET"):
        if rootclass == None:
            rootclass = Globals["Syntax"]
        return [ "    %s visit(%s syn, ARG1 arg1, ARG2 arg2) throws EXN;" % (ret, cl.jname())
            for cl in AllSyntaxClasses
            if cl.extends(rootclass)
            ]
    def genVisitors(this):
        # WARNING: if you add a visitor here, add it to 'accepts' too. 
        this.genVisitor(name="Visitor", useRet=True)
        this.genVisitor(name="Walker", useRet=False)
        this.genVanillaVisitor(name="VanillaVisitor", parent="Visitor", useRet=True)
        this.genVanillaVisitor(name="VanillaWalker", parent="Walker", useRet=False)
        this.genVisitor(name="VisitCmd", useRet=True, root="Cmd")
        this.genVanillaVisitor(name="VanillaVisitCmd", parent = "VisitCmd", useRet=True, root="Cmd")
        this.genVisitor(name="VisitAssignTarget", useRet=True, root="AssignTarget")
        this.genVisitor(name="VisitPat", useRet=True, root="Pat")
        this.genVisitor2(name="VisitPat2", useRet=True, root="Pat")
        this.genVisitor(name="VisitQueryAbstract", useRet=True, root="QueryAbstract")
        this.genVisitor(name="VisitQueryControl", useRet=True, root="QueryControl")
        this.genVanillaVisitor(name="VanillaVisitPat", parent="VisitPat", useRet=True, root="Pat")
        this.genVisitor(name="WalkPat", useRet=False, root="Pat")
        this.genVanillaVisitor(name="VanillaWalkPat", parent="WalkPat", useRet=False, root="Pat")
        for interf in AllInterfs:
            this.genInterfVisitor(interf, useRet = True)
            this.genInterfVisitor(interf, useRet = False)

    def genInterfVisitor(this, interf, useRet=True):
        name = interf.visitorName(useRet)
        visitorFilename = this.filename(name, package = this.visitorPackage)
        RET = "RET, " if useRet else ""
        ret = "RET" if useRet else "void"
        code = [
          "package %s;" % (".".join(this.visitorPackage)),
          WARNING,
          "import %s.*;" % this.thepackage(), 
          "import fisher.syn.core.*;",
          "import fisher.syn.interfaces.*;",
          "public interface %s<ARG, %s EXN extends Exception> {" % (name, RET),
          this.visitorInterfLines(ret, useRet, interf),
          "} "
          ]
        squishAndWrite(code, visitorFilename)

    def visitorInterfLines(this, ret, useRet, interf): 
        return [
          '''     %s visit(%s syn, ARG arg) throws EXN; ''' % (ret, cl.jname())
          for cl in interf.implementations
        ]
        
    def genVisitor(this, useRet=True, name="Visitor", root="Syntax"):
        visitorFilename = this.filename(name, package = this.visitorPackage)
        RET = "RET, " if useRet else ""
        ret = "RET" if useRet else "void"
        rootclass = Globals[root]
        code = [
          "package %s;" % (".".join(this.visitorPackage)),
          WARNING,
          "import %s.*;" % this.thepackage(), 
          "import fisher.syn.core.*;",
          "import fisher.syn.interfaces.*;",
          "public interface %s<ARG, %s EXN extends Exception> {" % (name, RET),
          this.visitorLines(ret=ret, rootclass = rootclass),
          "} "
          ]
        squishAndWrite(code, visitorFilename)
    def genVisitor2(this, useRet=True, name="Visitor", root="Syntax"):
        visitorFilename = this.filename(name, package = this.visitorPackage)
        RET = "RET, " if useRet else ""
        ret = "RET" if useRet else "void"
        rootclass = Globals[root]
        code = [
          "package %s;" % (".".join(this.visitorPackage)),
          WARNING,
          "import %s.*;" % this.thepackage(), 
          "import fisher.syn.core.*;",
          "import fisher.syn.interfaces.*;",
          "public interface %s<ARG1, ARG2, %s EXN extends Exception> {" % (name, RET),
          this.visitorLines2(ret=ret, rootclass = rootclass),
          "} "
          ]
        squishAndWrite(code, visitorFilename)
    def vanillaVisitorLines(this, name="VanillaVisitor", useRet=True, root=None):
        L = []
        ret = "RET" if useRet else "void"
        returns = "return" if useRet else ""
        rootclass = Globals[root] if root != None else Globals["Syntax"]
        for cl in AllSyntaxClasses:
            cname = cl.jname()
            su = cl.parent
            if not(cl.extends(rootclass)):
                continue
            if su == None or cl == rootclass:
                L.append("    public abstract %s visit(%s syn, ARG arg) throws EXN;" % (ret, cname))
            else:
                sname = su.jname()
                L.append("    public %s visit(%s syn, ARG arg) throws EXN {" % (ret, cname))
                L.append("       %s this.visit((%s)syn, arg);" % (returns, sname))
                L.append("       }")
        return L
                

    def genVanillaVisitor(this, name="VanillaVisitor", parent="Visitor", useRet = True, root=None):
        visitorFilename = this.filename(name, package = this.visitorPackage)
        RET = "RET, " if useRet else ""
        code = [
          "package %s;" % (".".join(this.visitorPackage)),
          WARNING,
          "import %s.*;" % this.thepackage(), 
          "import fisher.syn.interfaces.*;",
          "import fisher.syn.core.*;",
          ("public abstract class %s<ARG, %s EXN extends Exception>" % (name, RET)),
          "   implements %s<ARG,%sEXN>" % (parent, RET),
          "{",
          this.vanillaVisitorLines(name=name,useRet=useRet,root=root),
          "} "
          ]
        squishAndWrite(code, visitorFilename)

AllSyntaxClasses = [] # List of all syntactic forms.
AllInterfs = [] # List of all interfaces.

Exclude = ["start", 'end', 'actualCode'] # Fields that don't get toStringed.

class CLS:
    def __init__(this, name, parent, fields, mods = [], generate = True, tostr = None, extracode = [], aspat=None, implements=[], ctorSuperCall=None, details=None, isDeepCopyable=True, deepCopyFiddling=[]):
        global AllSyntaxClasses
        global Globals
        Globals[name] = this
        this.name = name
        this.parent = parent
        this.fields = fields
        this.mods = mods
        this.generate = generate
        this.tostr = tostr
        this.extracode = extracode
        this.aspat = aspat
        this.implements = implements
        this.isDeepCopyable = isDeepCopyable
        this.deepCopyFiddling = deepCopyFiddling
        if parent != None:
            for im in parent.implements:
                if not(im in implements):
                    implements.append(im)
        if implements != None: 
            for im in implements:
                im.implementations.append(this)
        this.ctorSuperCall = ctorSuperCall
        this.detailsCode = details
        this.allFields = parent.allFields + fields if parent != None else fields
        this.allFieldsButToks = parent.allFieldsButToks + fields if parent != None else fields
        AllSyntaxClasses.append(this)
    def isAbstract(this):
        return "abstract" in this.mods
    def extends(this, other):
        if this == other:
            return True
        if this.parent == None:
            return False
        else:
            return this.parent.extends(other)
    def __str__(this):
        if this.parent != None:
            return "CLS(%s extends %s with %s %s)" % (this.name, this.parent.name, strall(this.fields), this.mods)
        else:
            return "CLS(%s with %s %s)" % (this.name, strall(this.fields), this.mods)
    def jname(this):
        return this.name
    def imports(this):
        return [
        "import java.util.*;",
        "import fisher.syn.visitor.*;",
        "import fisher.syn.*;",
        "import fisher.statics.*;",
        "import fisher.syn.core.*;",
        "import fisher.syn.interfaces.*;",
        "import fisher.parser.Token;",
        "import fisher.syn.chart.*;",
        "import fisher.runtime.*;",
        "import fisher.statics.purity.PurityStatus;",
        ]
    def modifiers(this): 
        return " ".join(this.mods)
    def getCastytype(this):
        return this.name
    def fieldCode(this, gene):
        L = []
        for f in this.fields:
            L.append(f.code(this,gene))
        return L
    def ctorArgs(this): 
        L = []
        for f in this.allFields: 
            itstype = f.jtypename()
            itsname = f.jname()
            arg = "%s %s" % (itstype, itsname)
            L.append(arg)
        s = ", ".join(L)
        return s
    def accepts(this,gene):
        # WARNING: if you change this, change 'genVisitors' too.
        L= [
          this.acceptVisitor(gene, useRet=True, name="Visitor"), 
          this.acceptVisitor(gene, useRet = False, name="Walker"),
          this.acceptVisitor(gene, useRet=True, root="Cmd", name="VisitCmd"),
          this.acceptVisitor(gene, useRet=True, root="Pat", name="VisitPat"),
          this.acceptVisitor2(gene, useRet=True, root="Pat", name="VisitPat2"),
          this.acceptVisitor(gene, useRet=True, root="AssignTarget", name="VisitAssignTarget"),
          this.acceptVisitor(gene, useRet=False, root="Pat", name="WalkPat"),
          this.acceptVisitor(gene, useRet=True, root="QueryControl", name="VisitQueryControl"),
          this.acceptVisitor(gene, useRet=True, root="QueryAbstract", name="VisitQueryAbstract")          
        ]
        for interf in AllInterfs:
            if interf.isInstance(this): 
                vi = this.acceptInterfVisitor(useRet = True, interf=interf, name = interf.visitorName(True))
                L.append(vi)
                vi = this.acceptInterfVisitor(useRet = False, interf=interf, name = interf.visitorName(False))
                L.append(vi)
                # Structural sanity test: 
                if not(this in interf.implementations): 
                    print "Oh doom! %s is not in implementations of %s!" % (this.jname(), interf.jname())

        return L

    def acceptInterfVisitor(this, useRet=True, interf=None, name=None):
        RET = "RET, " if useRet else ""
        ret = "RET" if useRet else "void"
        returns = "return" if useRet else ""
        L = [
             "public <ARG, %s EXN extends Exception> %s accept(%s<ARG,%s EXN> vis, ARG arg) throws EXN {" % (RET, ret, name, RET),
             "   %s vis.visit((%s)this, arg);" % (returns, this.jname()),
             "}"
             ]
        return L
        

    def acceptVisitor(this,gene, useRet=True, root="Syntax", name="Visitor"):
        RET = "RET, " if useRet else ""
        ret = "RET" if useRet else "void"
        rootclass = Globals[root]
        returns = "return" if useRet else ""
        if this.extends(rootclass): 
           L = [
             "public <ARG, %s EXN extends Exception> %s accept(%s<ARG,%s EXN> vis, ARG arg) throws EXN {" % (RET, ret, name, RET),
             "   %s vis.visit((%s)this, arg);" % (returns, this.jname()),
             "}"
             ]
           return L
        else:
           return []
    def acceptVisitor2(this,gene, useRet=True, root="Syntax", name="Visitor"):
        RET = "RET, " if useRet else ""
        ret = "RET" if useRet else "void"
        rootclass = Globals[root]
        returns = "return" if useRet else ""
        if this.extends(rootclass): 
           L = [
             "public <ARG1, ARG2, %s EXN extends Exception> %s accept(%s<ARG1, ARG2,%s EXN> vis, ARG1 arg1, ARG2 arg2) throws EXN {" % (RET, ret, name, RET),
             "   %s vis.visit((%s)this, arg1, arg2);" % (returns, this.jname()),
             "}"
             ]
           return L
        else:
           return []
    def acceptWalker(this,gene):
        L = [
          "public <ARG, EXN extends Exception> void accept(Walker<ARG,EXN> vis, ARG arg) throws EXN {",
          "   vis.visit((%s)this, arg);" % this.jname(),
          "}"
          ]
        return L
    def constructor(this,gene):
        L = ["public %s(%s){" % (this.name, this.ctorArgs())]
        pca = ""
        for pf in this.parent.allFields:
            pca += pf.jname()
            if pf != this.parent.allFields[-1]:
                pca += ", "
        if this.ctorSuperCall == None: 
            L.append("    super(%s);" % pca)
        else:
            L.append(this.ctorSuperCall)
        for f in this.fields:
            L.append("    this.%s = %s;" % (f.jname(), f.jname()))
            L.append(f.addProgeny())
        L.append("}")
        return L
    def deepCopy(this):
        if this.isAbstract():
            return []
        L = ["public Syntax internalDeepCopy(Token start, Token end)  {"]
        # Gotta parallel the ctor.
        # args for super part of call
        pca = ""
        for pf in this.allFields:
            if pf.isStartOrEnd:
                pca += pf.jname()
            else: 
                pca += pf.deepCopier()
            if pf != this.allFields[-1]:
                pca += ", "
        L.append("Syntax copy = new %s(%s);" % (this.name, pca))
        L.append(this.deepCopyFiddling);
        L.append("return copy;")
        L.append("}")
        return L
    def details(this,gene):
        if this.detailsCode == None:
            return this.detailsNormal(gene)
        else:
            return "public String details() {return %s;}" % this.detailsCode
    def detailsNormal(this,gene):
        L = ["public String details(){"]
        L.append('return "%s("' % this.name)
        needsSpaceBefore = False
        for f in this.allFields: 
            if f.jname() not in Exclude:
                if needsSpaceBefore:
                    L.append('    + " "')
                L.append('    + "%s=" + %s' % (f.nick, f.tostringy()))
                needsSpaceBefore = True
        L.append('    +")";')
        L.append("}")
        return L
    def strdet(this,gene):
        Formals = []
        BodyBits = []
        for f in this.allFields: 
            j = f.jname()
            if j not in Exclude: 
                Formals.append("Object %s" % j)
                BodyBits.append("\"%s=\" + %s" % (f.nick,j))
        if BodyBits == []:
            BodyBits.append("\"\"")
        Code = ["public static String detstr(", 
                ", ".join(Formals),
                ") {",
                "return \"%s(\" + " % (this.name),
                "+\" \"+" . join(BodyBits),
                "+ \")\";}"
                ]
        return Code
    def tostring(this, gene): 
        if this.tostr == None: 
            return "public String toString() {return this.details();}"
        else:
            return "public String toString() {return %s;}" % this.tostr
    def asPat(this):
        if this.aspat != None: 
            return ['    public Syntax asPat() throws NotAPat{', '       return %s;' %this.aspat, '    }']
        else:
            return []
    def fieldSpecials(this, gene):
        L = []
        for f in this.fields:
            L.append(f.specialCode(this))
        return L
    def implementses(this): 
        if this.implements == []:
            return ""
        else:
            return "implements " + (", ".join([s.jname()  for s in this.implements]))
    def getChild(this, gene): 
        L = [
          ''' 
          public Object getChild(int field, int index) { 
             Object ret = null;
             switch(field) {
          '''
          ]
        i = 0;
        for f in this.allFieldsButToks:
            line = "                     case %s: ret = %s; %s; break;" % (
               i, 
               f.getChildRetVal(),
               f.getChildNullCheck());
            L.append(line);
            i += 1
        L.append(['''
           default: fisher.util.Doom.internalCatastrophe("bad field number in getChild", this, field, index);
           }
           return ret;
        }
        '''])
        return L
    def synPtrs(this, gene):
        L = ['''
            public List<SynPtr> synPtrs() {
               List<SynPtr> ptrs = new ArrayList<SynPtr>();
            ''']
        i = 0
        pre = "              "
        for f in this.allFieldsButToks:
            fn = f.jname()
            l1 = pre + "// %s : %s" % (i, fn)
            L.append(l1);
            if f.isChild: 
                L.append(f.synPtrs(i, pre))
            i = i + 1;
        L.append(pre + "return ptrs;\n" + pre + "}")
        return L 
    def internalReplace(this,gene):
        L = ['''
        public void internalReplace(int field, int index, Object newval) {
	   switch (field) {
        ''']
        pre = "		"
        i = 0
        for f in this.allFieldsButToks:
            line = "%s case %s: %s break;" % (pre, i, f.internalReplaceBody())
            i = i + 1
            L.append(line)
        L.append('''
        default:
         fisher.util.Doom.internalCatastrophe("bad field number in internalReplace", this, field, index, newval);
		}
	}
        ''')
        return L
    def genericName(this):
        return '''
          public String genericName(){return "%s";}
        ''' % (this.name)
    def fieldExtraCode(this):
        L = []
        for f in this.fields:
            fx = f.extracodef()
            L = L + fx
        return L
    def gen(this, gene):
        if this.generate: 
            filename = gene.filename(this.name)
            code = [
               "package %s;" % (gene.thepackage()),
               WARNING,
               this.imports(),
               "public %s class %s extends %s %s {" % (this.modifiers(), this.name, this.parent.name, this.implementses()),
               this.fieldCode(gene),
               this.constructor(gene),
               this.accepts(gene),
               this.details(gene),
               this.strdet(gene),
               this.tostring(gene),
               this.fieldSpecials(gene),
               this.getChild(gene),
               this.synPtrs(gene),
               this.internalReplace(gene),
               this.asPat(),
               this.genericName(),
               this.deepCopy(),
               this.fieldExtraCode(),
               this.extracode,
               "}"
            ]
            squishAndWrite(code, filename)
        else: 
            pass

def squishAndWrite(code, filename):
            flatcode = squish(code)
            file = open(filename, "w")
            file.write(flatcode)
            file.flush()
            file.close()



def contents(filename):
  f = open(filename, "r")
  r = f.read()
  f.close()
  return r


class CORE:
    def __init__(this, name, castytype=None, isDeepCopyable=True):
        this.name = name
        this.castytype = castytype if castytype != None else name
        this.isDeepCopyable = isDeepCopyable
    def __str__(this): 
        return "%s" % this.name
    def jname(this):
        return this.name
    def getCastytype(this):
        return this.castytype
    
class FIELD:
    def __init__(this, aname, atype, isChild = True, isFinal = False, nullable=False, nick=None, isStartOrEnd = False, extracode=[]):
        this.fname = aname
        this.ftype = atype
        this.isChild = isChild
        this.isFinal = isFinal
        this.isStartOrEnd = isStartOrEnd
        this.nullable = nullable
        this.extracode = extracode
        if nick == None:
            this.nick = aname
        else:
            this.nick = nick
    def __str__(this): 
        return "FIELD(%s %s)" % (this.ftype.jname(), this.fname)
    def code(this, cls, gene): 
        return (''' public %s %s %s; ''' % (this.finality(), this.ftype.jname(), this.fname))
    def finality(this):
        if this.isFinal:
            return "final"
        else:
            return ""
    def jtypename(this):
        return this.ftype.jname()
    def extracodef(this):
        return this.extracode
    def jtype(this):
        return this.ftype
    def jname(this):
        return this.fname
    def tostringy(this):
        return "Syntax.detailsome(this.%s)" % this.fname
    def addProgeny(this): 
        if this.isChild: 
           return "    if(%s != null) children.add(%s);" % (this.jname(),this.jname())
        else:
           return []
    def specialCode(this, cls):
        return []
    def getChildRetVal(this):
        return "this.%s" % this.jname()
    def getChildNullCheck(this):
        if this.nullable: 
            return ""
        else:
            return '''
            {if (ret == null) 
                fisher.util.Doom.internalCatastrophe("Non-nullable field (%s) was null", this, field, index);}                
            ''' % (this.jname());
    def synPtrs(this, i, pre):
        return "%sif (%s != null) ptrs.add(new SynPtr(this, %s, -1)); " % (
            pre, this.jname(),  i)
    def internalReplaceBody(this):
        a = this.jname()
        return "this.%s = (%s) newval;" % (a, this.ftype.getCastytype())
    def deepCopier(this):
        if this.ftype.isDeepCopyable:
            return "\n(%s == null ? null : (%s)%s.internalDeepCopy(start,end))" % (this.jname(), this.ftype.getCastytype(), this.jname())
        else:
            return this.jname()

class LIST:
    def __init__(this, sing, pl, indiType, isChild = True, nick=None,extracode=[]):
        this.sing = sing
        this.pl = pl
        this.indiType = indiType
        this.isStartOrEnd = False
        this.isChild = isChild
        this.extracode = extracode
        if nick == None:
            this.nick = this.pl
        else:
            this.nick = nick
    def __str__(this):
        return "LIST(%s/%s List<%s>)" % (this.sing, this.pl, this.indiType.jname())
    def extracodef(this):
        return this.extracode
    def jtypename(this):
        return this.jtype()
    def jtype(this): # This is confusing! jtype sometimes is a type, sometimes a string. Bad bad.
        return "List<%s>" % (this.indiType.name)
    def jname(this): 
        return this.pl
    def code(this, cls, gene): 
        return "public final %s %s;" % (this.jtype(), this.jname())
    def tostringy(this):
        return '"["+Syntax.sepDetails(this.%s, ",")+"]"' % this.jname()
    def addProgeny(this):
        if this.isChild:
#            return "    children.addAll(%s);" % this.jname()
            return "for(Object o : %s){children.add((Syntax)o);}" % this.jname()
        else:
            return []
    def specialCode(this, cls):
        return []
    def getChildRetVal(this): 
        return "this.%s.get(index)" % this.jname()
    def getChildNullCheck(this): 
        return ""
    def synPtrs(this, i, pre):
        return (
           pre + ("for(int i = 0; i < this.%s.size(); i++) {" % this.jname()) +
           pre + "   ptrs.add(new SynPtr(this, %s, i));}" % i
           )
    def internalReplaceBody(this):
        return "this.%s.set(index, (%s) newval);" % (this.jname(), this.indiType.jname())
    def deepCopier(this): 
        return "(List<%s>)(deepCopyList(%s, start, end))" % (
           this.indiType.jname(), this.jname())

        
class INTERF:
    def __init__(this, name): 
        global AllInterfs
        global Globals
        this.name = name
        this.implementations = []
        Globals[name] = this
        AllInterfs.append(this)
    def jname(this):
        return this.name
    def isInstance(this,cls):
        for impl in this.implementations:
            if cls.extends(impl):
                return True
        return False
    def visitorName(this, useRet):
        walvis = "Visitor" if useRet else "Walker" 
        return this.name + walvis

def squish(L, sep="\n"):
    if type(L) == list:
        s = ""
        for i in range(len(L)): 
            s += squish(L[i],sep)
            if i < len(L)-1:
                s += sep
        return s
    else:
        return str(L)


def strall(L, sep=", "):
    if L == []:
        return ""
    elif len(L) == 1:
        return str(L[0])
    else:
        return str(L[0]) + sep + strall(L[1:], sep)

gene = Gene("/Users/bard/thorn/fisherws/fisher/src/", ["fisher", "syn"])


#######################################################
#
# Syntax classes get defined here. 
#
#######################################################


Abstract = "abstract"
Object = CORE("Object",isDeepCopyable=False) # Java objects
Token = CORE("Token")
Boolean = CORE("boolean", castytype="Boolean", isDeepCopyable=False)
Int = CORE("int", castytype="Integer", isDeepCopyable=False)
String = CORE("String", isDeepCopyable=False)
Op = CORE("Op",isDeepCopyable=False)
OpAB = CORE("OpAB",isDeepCopyable=False)
ComparisonOp = CORE("ComparisonOp",isDeepCopyable=False)
ColAccess = CORE("ColAccess",isDeepCopyable=False)
ColSpecial = CORE("ColSpecial",isDeepCopyable=False) 
Visibility = CORE("Visibility",isDeepCopyable=False)
SortOrder = CORE("SortOrder",isDeepCopyable=False)
QuoteStyle = CORE("QuoteStyle",isDeepCopyable=False)
PurityStatus = CORE("PurityStatus", isDeepCopyable=False)

ClassMember = INTERF("ClassMember")
ObjectMember = INTERF("ObjectMember")
ProcMember = INTERF("ProcMember")
TableMember = INTERF("TableMember")
LocalMember = INTERF("LocalMember")
ModuleFileMember = INTERF("ModuleFileMember")
Classlike = INTERF("Classlike")
Puretic = INTERF("Puretic")
ComponentInfo = INTERF("ComponentInfo")

ANYMEMBER = [ClassMember, ObjectMember, ProcMember, TableMember, ModuleFileMember, LocalMember]

Syntax = CLS("Syntax", None, 
   [FIELD("start", Token,  isStartOrEnd = True), FIELD("end", Token, isStartOrEnd = True)],  
   generate=False, mods=[Abstract])
Syntax.allFieldsButToks = []

Id = CLS("Id", Syntax, [], generate=False)
ID = FIELD("id", Id)

# Most major syntactic bits are Cmds
Cmd = CLS("Cmd", Syntax, [], mods = [Abstract],
      extracode = [
         "public boolean explike(){return false;}"
      ]
      )

EXP = FIELD("exp", Cmd)

EXPLIKE = "public boolean explike(){return true;}"

PURITY = FIELD("isMarkedPure", Boolean, isChild=False,
     extracode=['''
        public boolean isMarkedPure() {return this.isMarkedPure;}
     ''']
     )
PURETICS = '''
     private PurityStatus purityStatus = PurityStatus.UNCHECKED;
     public PurityStatus purityStatus() { return this.purityStatus;}
     public void setPurityStatus(PurityStatus np) { this.purityStatus = np; }
     '''

# Pat = the class of patterns.
Pat = CLS("Pat", Syntax, [], mods = [Abstract])

# ELLIPSIS = Java string for ellipsis
ELLIPSIS = '"\\u2026"'

# Args = a list of arguments.
ARGS = LIST("arg", "args", Cmd)
ARGS_NotChild = LIST("arg", "args", Cmd, isChild=False)

# QualName = qualified name.  One or more ids.
QualName = CLS("QualName", Syntax, [LIST("id", "ids", Id)],
   tostr = ''' sep(ids, ".") ''',
   extracode = ['''
     public QualName butlast() {
        List<Id> nids = new ArrayList(ids.size()-1);
        if (ids.size() < 2) {
          throw new RuntimeException("Can't take butlast of empty or singleton QualName.");
        }
        Id last = null;
        for(int i = 0; i < ids.size()-1; i++) {
          last = ids.get(i);
          nids.add(last);
          }
        return new QualName(this.start, last.end, nids);        
     }
     ''',
     '''
     public Id last() { return ids.get(ids.size()-1);}
     ''',
     '''
     public boolean equals(Object o) {
       if(o instanceof QualName) {
         QualName qo = (QualName) o;
         return (this.ids.equals(qo.ids));
       }
       else return false;
     }
     public int hashCode(){
       int i = 0;
       for(Id id : this.ids) i += id.hashCode();
       return i;
     }
     '''
   ]
   )

# Formal = structure of a formal argument

Formals = CLS("Formals", Syntax, [LIST("formal", "formals", Pat)], 
  tostr = '''"(" + sep(formals, ",") + ")" ''',
  details = '''"(" + sepDetails(formals, ",") + ")"  '''
  )

FORMALS = FIELD("formals", Formals)

# Type constraint

TypeConstraint = CLS("TypeConstraint", Syntax, 
       [FIELD("typename", Id)],
       tostr = '''typename.toString()''')

TypeConstraints = CLS("TypeConstraints", Syntax,
    [LIST("constraint", "constraints", TypeConstraint)],
    tostr = ''' 
       (constraints.isEmpty() 
        ? "" 
        : ":" + sep(constraints, " & "))''',
    extracode = ['''
        public List<Id> ids() {
          List<Id> ids = new ArrayList<Id>(constraints.size());
          for(TypeConstraint tc : constraints) {
             ids.add(tc.typename);
          }
          return ids;
        }
    ''']
    )

################################################################

Literal = CLS("Literal", Cmd, [FIELD("value", Object, isChild=False)],
   tostr = '''
     value == null ? "null" :
     value instanceof String ? quote(value.toString()) :  value.toString()
   ''',
   details = '''"(" + ( value == null ? "<null>" : value.toString() ) + ")"''',
   extracode = [EXPLIKE,
   '''
      public Thing thing;
      public static String quote(String s) {
         if (s.contains("\\n")) return "'''" + s + "'''";
         if (! s.contains("\\"")) return "\\"" + s + "\\"";
         if (! s.contains("'")) return "'" + s +"'";
         return "\\"" + s.replaceAll("\\"", "\\\\\\"") + "\\"";
      }
   ''']
   )

AbstractStringBit = CLS("AbstractStringBit", Syntax, [], mods=[Abstract],
   extracode = [
     '''
        public abstract void formatted(QuoteStyle qs, StringBuffer sb);
     '''
     ]
   )

StringBitText = CLS("StringBitText", AbstractStringBit, 
     [FIELD("value", String, isChild=False)],
     tostr='''"'"+value.toString()+"'"''' ,
     extracode = [
     '''
        public void formatted(QuoteStyle qs, StringBuffer sb) { qs.formatString(value, sb); }
     '''
     ]     
     )

StringBitVar = CLS("StringBitVar", AbstractStringBit,
     [FIELD("exp", Cmd),
      FIELD("backquoted", Boolean, isChild=False)],
     tostr = '''
        backquoted ? "`" + exp.toString() + "`" : exp.toString()
        ''',
     extracode = [
     '''
        public void formatted(QuoteStyle qs, StringBuffer sb) { qs.formatVar(exp, backquoted, sb); }
     '''
     ]     
     )
      

StringWithInterpolations = CLS("StringWithInterpolations", Cmd,
   [LIST("bit", "bits", AbstractStringBit),
    FIELD("quoteStyle", QuoteStyle, isChild=False)],
   tostr='''quoteStyle.format(bits)''')

VarExp = CLS("VarExp", Cmd, [FIELD("id", Id)],
   tostr = '''id.toString()''',
   details = '''"<" + id.toString() + ">"''',
   extracode = [EXPLIKE]
   )

This = CLS("This", Cmd, [], tostr = '"this"', extracode=[EXPLIKE])

Parens = CLS("Parens", Cmd, [FIELD("exp", Cmd)],
   tostr = ''' "(" + exp + ")" ''',
   details = ''' "(" + exp.details() + ")" ''',
   extracode = [EXPLIKE]
   )

Valof = CLS("Valof", Cmd,   
      [LIST("stmt", "stmts", Cmd),
       FIELD("innerFrame", Boolean, isChild=False)
      ],
   tostr = ''' 
       (innerFrame ? "valofInner{" : "valof{")
       + sepStmt(stmts, " ") + "} " ''',
   extracode = [EXPLIKE]
   ) 


AssignTarget = CLS("AssignTarget", Syntax, [], mods=[Abstract])

AssignToId = CLS("AssignToId", AssignTarget, [FIELD("id", Id)], 
   tostr = ''' id.toString() '''
   )

AssignToField = CLS("AssignTofield", AssignTarget, [FIELD("target", Cmd), FIELD("field", Id)],
   tostr = ''' "" + target + "." + field '''
   )

AssignToSubscripted = CLS("AssignToSubscripted", AssignTarget, 
   [FIELD("array", Cmd), LIST("subscript", "subscripts", Cmd)],
   tostr = ''' array + "(" + sep(subscripts, ", ") + ")" '''
   )

AssignToMap  = CLS("AssignToMap", AssignTarget, 
   [FIELD("map", Cmd), LIST("subscript", "subscripts", Cmd)],
   tostr = ''' map + "[" + sep(subscripts, ", ") + "]" '''
   )

AssignToFieldOfSubscripted = CLS("AssignToFieldOfSubscripted", AssignTarget, 
   [FIELD("array", Cmd), LIST("subscript", "subscripts", Cmd), FIELD("field", Id)],
   tostr = ''' array + "(" + sep(subscripts, ", ")  + ")" + "." + field'''
   )



Assign = CLS("Assign", Cmd, [LIST("l", "lhs", AssignTarget), LIST("r", "rhs", Cmd)],
   tostr = '''sep(lhs, ", ") + " := " + sep(rhs, ", ")''',
   extracode = [EXPLIKE]
   )

Bind = CLS("Bind", Cmd, [FIELD("pat", Pat), FIELD("exp", Cmd, nullable=True)], # 
   tostr = ''' pat + 
     (exp == null ? "" : " = " + exp) ''',
   extracode = [EXPLIKE,
     '''
        public boolean reallyAnAssignmentToAPatVariable = false;
        public boolean reallyAnInitializingAssignmentToAValField = false;
     '''],
   implements = [ModuleFileMember, ClassMember, ObjectMember, ProcMember, LocalMember]   
   )

VarDecl = CLS("VarDecl", Cmd, 
            [FIELD("var", Id), 
             FIELD("init", Cmd, nullable=True),
             FIELD("typeConstraints", TypeConstraints)
             ],
   tostr = ''' "var " + var + 
           typeConstraints + 
           (init == null ? ";" : " := " + init + ";")
           ''',
   implements = [ModuleFileMember, ClassMember, ObjectMember, ProcMember, LocalMember]
   )

OpExp = CLS("OpExp", Cmd, [FIELD("op", Op, isChild=False), 
                           LIST("operand", "operands", Cmd)],
   tostr = '''
     op.fixity.format(operands, op)
     ''',
   details = '''      op.fixity.formatDetails(operands, op) ''',
   extracode = [EXPLIKE]   
   )

OpABExp = CLS("OpABExp", Cmd, 
   [FIELD("target", AssignTarget), 
    FIELD("opab", OpAB, isChild=False), 
    FIELD("amount", Cmd)],
   tostr = ''' "" + target + opab + amount ''',
   extracode = [EXPLIKE]
   )

TypedExp = CLS("TypedExp", Cmd,
   [FIELD("exp", Cmd),
    FIELD("type", QualName)],
   tostr = ''' exp + ":" + type '''
   )


ListForGroup = CLS("ListForGroup", Cmd,
   [FIELD("exp", Cmd)],
   tostr = ''' %s + exp  ''' % (ELLIPSIS)
   )

# Comparisons

ComparisonBit = CLS("ComparisonBit", Syntax,
   [FIELD("comparison", ComparisonOp, isChild=False), FIELD("to", Cmd)],
   tostr = '''" " + comparison + " " + to'''
   )     
        
Comparison = CLS("Comparison", Cmd, 
   [FIELD("first", Cmd), LIST("rest1", "rest", ComparisonBit)],
   tostr = '''first + sep(rest, "")''',
   extracode = [EXPLIKE]
   )

MatchExp = CLS("MatchExp", Cmd,
   [FIELD("subject", Cmd), FIELD("pat", Pat)],
   tostr = ''' subject + " ~ " + pat '''
   )
   

# Pattern stuff.

ItExp = CLS("ItExp", Cmd, [], tostr='"it"',
   extracode = [''' 
   ''']
 );

# Stuff that doesn't work as an Exp, existing only to give Exp-ish syntax
# to patterns

ForPatternOnly = CLS("ForPatternOnly", Cmd, [], mods=[Abstract],
     tostr = '''"Wrong!(" + this.details() + ")"'''
     )

WildcardExp = CLS("WildcardExp", ForPatternOnly, [])

DotMethodCallExp = CLS("DotMethodCallExp", ForPatternOnly, 
     [FIELD("methodName", Id),
      FIELD("subpat", Cmd, nullable=True)
     ])

InterpolationExp = CLS("InterpolationExp", ForPatternOnly, [FIELD("exp", Cmd)]
  )

EvalTestExpExp = CLS("EvalTestExpExp", ForPatternOnly, [FIELD("exp", Cmd)])

ExpExtract = CLS("ExpExtract", ForPatternOnly, 
   [FIELD("patname", QualName), 
    LIST("input", "inputs", Cmd),
    LIST("subpat", "subpats", Cmd)],
   tostr = ''' patname + "(" + 
      sep(subpats, ", ") + ")" '''
    )



# Testing constructs

Probe = CLS("Probe", Cmd,
   [FIELD("id", Id, nullable=True), LIST("exp", "exps", Cmd),
    FIELD("count", Int,  isChild = False)
    ],
   tostr = '''"~!@" + 
     (id == null ? "" : id) 
     + "(" + sep(exps, ", ") + ")"
     + (count >= 0 ? "@!~" + count : "")
     ''',
   extracode = [
     ''' public fisher.test.Prober prober; ''',
     ''' public Object scratch;''',
     EXPLIKE,
   ],
   deepCopyFiddling = [
     ''' ((Probe)copy).prober = this.prober; '''
   ]
   )



# Lists

ListBit = CLS("ListBit", Syntax, [], mods = [Abstract])

ListBitExp = CLS("ListBitExp", ListBit, [FIELD("exp", Cmd)],
   tostr = ''' exp.toString() '''
   )

ListBitEllip = CLS("ListBitEllip", ListBit,  [FIELD("exp", Cmd)],
   tostr = ''' exp + %s ''' % ELLIPSIS
   )

ListCtor = CLS("ListCtor", Cmd, [LIST("bit", "bits", ListBit)],
   tostr = ''' "[" + sep(bits, ", ") + "]"''',
   extracode = [EXPLIKE]
   )

# Single-bodied (non-matching) anonymous functions
MonoBody = CLS("MonoBody", Syntax, [
         FIELD("id", Id, nullable=True),
         FORMALS, 
         FIELD("from", Pat, nullable=True),
         FIELD("envelope", Pat, nullable=True),
         FIELD("hasPrio", Boolean, isChild = False),
         FIELD("prio", Int, isChild = False), 
         FIELD("body", Cmd),
         FIELD("checked", Boolean, isChild = False),
         PURITY
         ],
   implements = [Puretic],
   tostr = '''
     (id == null ? "" : id +
       (formals.toString().startsWith("(") ? "" : " ")
       )
     + formals  
     + (from == null ? "" : " from " + from)
     + (envelope == null ? "" : " envelope " + envelope)
     + (hasPrio ? " prio " + prio : "")
     + (checked ? " checked " : "")
     + " = " + stmt(body)''',
   extracode = [
     '''
     public String funname() {return id == null ? null : id.str(); }
     public Id idOfName(){return id;}
     ''',
     PURETICS
   ]
   )

FunBody = CLS("FunBody", Syntax, 
        [
          LIST("funbody", "funbodies", MonoBody),
          PURITY
        ],
   tostr = ''' sep(funbodies, " | ") ''',
   implements = [Puretic],
   extracode = [
     '''
     public Id id() {
       return funbodies.get(0).id;
     }
     public String funBodyForExp() {
       return sepExp(funbodies, " | ");
     }
     public Id idOfName(){return this.id();}
     ''',
     PURETICS
   ]
   )

AnonFun = CLS("AnonFun", Cmd, [FIELD("fun", FunBody),PURITY],
   tostr = ''' "fn " + fun.funBodyForExp() ''',
   implements = [Puretic],
   extracode = [EXPLIKE, PURETICS,
   '''
     public Id idOfName() {return null;}
   '''
   ]
   )

MethodCall = CLS("MethodCall", Cmd, [FIELD("target", Cmd), FIELD("method", Id), ARGS],
   tostr = '''target + "." + method + "(" + sep(args, ", ") + ")" ''',
   extracode = [EXPLIKE, 
   '''
     public MethodSig sig;
   '''],
   deepCopyFiddling = ['''
     ((MethodCall)copy).sig = this.sig;
   ''']
   )

FunCall = CLS("FunCall", Cmd, [FIELD("function", Cmd), ARGS],
   tostr = ''' function +  "(" + sep(args, ", ") + ")" ''',
   extracode = [EXPLIKE]
   )

BracketCall = CLS("BracketCall", Cmd, [FIELD("function", Cmd), ARGS],
   tostr = ''' function +  "[" + sep(args, ", ") + "]" ''',
   extracode = [EXPLIKE]
   )




FieldRef = CLS("FieldRef", Cmd, [FIELD("target", Cmd), FIELD("field", Id)],
   tostr = '''target + "." + field''',
   extracode = [EXPLIKE]
   )

SuperThingie = CLS("SuperThingie", Cmd, 
   [FIELD("cls", QualName, nullable=True), 
    ARGS],
   mods=[Abstract],
   extracode = [EXPLIKE, 
   '''
      public ClassStatic superClassStatic = null;
      public Seal sealForSuperclass = null;
   ''']
)             

SuperCall = CLS("SuperCall", SuperThingie, 
   [FIELD("method", Id)],
   tostr = '''
      cls == null ? "super." + method + "(" + sep(args, ", ") + ")"
       : "super@" + cls + "." + method + "(" + sep(args, ", ") + ")"
      ''',
   extracode = [EXPLIKE, 
   '''
     public MethodSig sig;
   '''],
   deepCopyFiddling = ['''
     ((SuperCall)copy).sig = this.sig;
   ''']

   )


SuperCtorCall = CLS("SuperCtorCall", SuperThingie,
   [],
   tostr = ''' 
     (cls == null ? "new" : "new@" + cls)
     + "(" + sep(args, ", ") + ")"
   ''',
   extracode = [
   '''
     public boolean isCallToADifferentCtorForThis() { 
        return cls == null;
     }
     public ClassStatic classStaticForCurrentClass = null; // only used if isCallToADifferentCtorForThis
   '''
   ]
   )

# A little syntactic hierarchy for the parsed-but-unassimilated things that can follow an expression
PostExp = CLS("PostExp", Syntax, [], mods=[Abstract])

PostExpDotId = CLS("PostExpDotId", PostExp, [FIELD("id", Id)],
   tostr = ''' "." + id '''
   )

PostExpArgs = CLS("PostExpArgs", PostExp, [LIST("arg", "args", Cmd)],
   tostr = ''' "(" + sep(args, ", ") + ")" '''
   )            

PostExpBracketArgs = CLS("PostExpBracketArgs", PostExp, [LIST("arg", "args", Cmd)],
   tostr = ''' "[" + sep(args, ", ") + "]" '''
   )            


################################################################

If = CLS("If", Cmd, 
   [FIELD("test", Cmd), FIELD("Then", Cmd), 
    FIELD("Else", Cmd), FIELD("reallyUnless", Boolean, isChild=False, nick="un"),
    FIELD("isExp", Boolean, isChild=False)
    ],
   tostr = '''
     isExp ? (
       asExp(Then)
       + (reallyUnless ? "unless " : "if ") + test 
       + " else " 
       + asExp(Else)
      )
    : (
       (reallyUnless ? "unless (" : "if (") + test + ")" 
        + asStmt(Then) + ""
      + (Else == null ? "" : " else " + asStmt(Else) + "")
      )
    ''' 
   )

Return = CLS("Return", Cmd, [FIELD("exp", Cmd, nullable=True)],
  tostr = ''' "return" + (exp == null ? "" : " " + exp) + ";"'''
  )

LabellableLoop = CLS("LabellableLoop", Cmd,
   [FIELD("label", Id, nullable=True), ],
    mods=[Abstract]  ,
    extracode = [ '''
     public Seal loopControlSeal = null;
    ''']
    )

Signature = CLS("Signature", LabellableLoop,
   [FIELD("signature", Int, isChild=False),
    FIELD("body", Cmd)],
   tostr = '''
     (label == null ? "" : label + ":")
   + "signature(" + signature + ")" + body
   '''
   )


While = CLS("While", LabellableLoop,
   [FIELD("test", Cmd), 
    FIELD("body", Cmd),
    FIELD("reallyUntil", Boolean, isChild=False, nick="un"),
    FIELD("reallyDo", Boolean, isChild=False, nick="do"),
    ],
   tostr= '''this.mystr()''',
   extracode=[
    '''
       private String mystr() {
         String lbl = (label == null ? "" : label + ": ");
         String ctl = (reallyUntil ? "until(" : "while(") + test + ")";
         if (reallyDo) 
            return lbl +  "do " + body + " " + ctl;
         else
            return lbl + ctl + body;
       }
       
    '''
    ]
   )

For = CLS("For", LabellableLoop, 
    [FIELD("pat", Pat),
     FIELD("list", Cmd),
     FIELD("body", Cmd),
     FIELD("inquisitive", Boolean, isChild=False)
     ],
    tostr = '''
      (label == null ? "" : label + ": ") + 
      "for (" + pat + 
      (inquisitive ? " <~ " : " <- ")
      + list + ")" + stmt(body)
      '''
    )


Seq = CLS("Seq", Cmd, [LIST("cmd", "cmds", Cmd)],
   tostr = '''"{" + sepStmt(cmds, " ") + "} "''',
   details = '''"{" + sepDetails(cmds, " ") + "}"''')

Skip = CLS("Skip", Cmd, [], 
   tostr = ''' ";" '''
   )


FunDecl = CLS("FunDecl", Cmd, 
        [
          FIELD("name", Id), 
          FIELD("funbody", FunBody),
          PURITY
        ],
   tostr = ''' endingInSemi("fun " +  (funbody)) ''',
   implements = [ModuleFileMember, ProcMember, Puretic],
   extracode = [PURETICS, 
   '''
     public Id idOfName(){return name;}
   ''']
   )



Break = CLS("Break", Cmd, [FIELD("id", Id, nullable=True)],
   tostr = '''"break"
      + (id == null ? ";" : " " + id + ";")
      ''',
   extracode = ['''
      public Seal loopSeal;
   ''']
   )
      
Continue = CLS("Continue", Cmd, [FIELD("id", Id, nullable=True)],
   tostr = '''"continue"
      + (id == null ? ";" : " " + id + ";")
      ''',
   extracode = ['''
      public Seal loopSeal;
   ''']
   )
      

################################################################
# Matching, Cases
################################################################

# Pattern prio exp
Case = CLS("Case", Syntax,
   [FIELD("pat", Pat),  
    FIELD("from", Pat, nullable=True),
    FIELD("envelope", Pat, nullable=True),
    FIELD("hasPrio", Boolean, isChild = False),
    FIELD("prio", Int, isChild = False), 
    FIELD("checked", Boolean, isChild = False),
    FIELD("body", Cmd)],
   tostr = '''
      pat 
      + (from == null ? "" : " from " + from )
      + (envelope == null ? "" : " envelope " + envelope )
      + (!hasPrio ? "" : " prio " + prio)
      + (checked ? " checked " : "")
      + " = " + asStmt(body)
   '''
   )

Match = CLS("Match", Cmd, [FIELD("subject", Cmd), LIST("acase", "cases", Case)],
   tostr = '''"match " + subject + "{" + sep(cases, " | ") + "} "'''
   )

Try = CLS("Try", Cmd, 
    [FIELD("body", Cmd), LIST("acase", "cases", Case), FIELD("fin", Cmd)],
    tostr = ''' "try " + body + 
              (cases.isEmpty() ? "" : " catch {" + sep(cases, " | ") + "} ")
              + (fin == null ? "" : " finally " + fin)
              '''
    )

Throw = CLS("Throw", Cmd,
    [FIELD("exn", Cmd)],
    tostr = ''' "throw " + exn + ";" '''
    )

################################################################
# Patterns
################################################################

PatVar = CLS("PatVar", Pat, [FIELD("id", Id)],
   tostr = '''id.toString()''',
   details = '''"<~" + id.toString() + "~>"''',
   extracode = [
   '''
     public boolean sameNameAs(PatVar other) { return this.id.str().equals(other.id.str());}
   ''']
   )
   
PatLiteral = CLS("PatLiteral", Pat,   [FIELD("value", Object, isChild=False)],
   tostr = '''value == null ? "null" : value.toString()''',
   details = '''"$(" + value.toString() + ")"''',
   extracode = ['''
     public Thing thing;
   '''],
   deepCopyFiddling = ['''
     ((PatLiteral)copy).thing = thing;
     ''']
   )

PatListBit = CLS("PatListBit", Syntax, [], mods = [Abstract])

PatListBitExp = CLS("PatListBitExp", PatListBit, [FIELD("pat", Pat)],
   tostr = ''' pat.toString() '''
   )

PatListBitEllip = CLS("PatListBitEllip", PatListBit,  [FIELD("pat", Pat)],
   tostr = ''' pat + %s ''' % ELLIPSIS
   )

PatListCtor = CLS("PatListCtor", Pat, [LIST("bit", "bits", PatListBit)],
   tostr = ''' "[" + sep(bits, ", ") + "]"''',
   )

PatWildcard = CLS("PatWildcard", Pat, [], tostr ='"_"')

PatInterpolation = CLS("PatInterpolation", Pat, [FIELD("exp", Cmd)],
   tostr = ''' "$(" + exp + ")" '''
   )


PatEvalTestExp = CLS("PatEvalTestExp", Pat, [FIELD("exp", Cmd)],
   tostr = ''' "(" + exp + ")?" '''
   )

PatAnd = CLS("PatAnd", Pat, [LIST("subpat", "subpats", Pat)],
   tostr = '''sep(subpats, " && ")'''
   )

PatOr = CLS("PatOr", Pat, [LIST("subpat", "subpats", Pat)],
   tostr = '''sep(subpats, " || ")'''
   )

PatNot = CLS("PatNot", Pat, [FIELD("subpat", Pat)],
   tostr = ''' "!" +  subpat'''
   )

PatNonNull = CLS("PatNotNull", Pat, [FIELD("subpat", Pat)],
   tostr = ''' "+" + subpat '''
   )

PatMatchSomethingElse = CLS("PatMatchSomethingElse", Pat, 
   [FIELD("exp", Cmd), FIELD("pat", Pat)],
   tostr = ''' exp + " ~ " + pat '''
   )

PatExtract = CLS("PatExtract", Pat,
   [FIELD("patname", QualName), 
    LIST("subpat", "subpats", Pat)],
   tostr = ''' patname + "(" + 
      sep(subpats, ", ") + ")" '''
   )

PatTypeTest = CLS("PatTypeTest", Pat, 
   [FIELD("subpat", Pat),
    FIELD("shouldBe", QualName)],
   tostr = ''' subpat + ":" + shouldBe '''
   )
   

PatRecordField = CLS("PatRecordField", Syntax, [FIELD("id", Id), FIELD("pat", Pat)],
   tostr = ''' id + ":" + pat '''
   )

PatRecordCtor = CLS("PatRecordCtor", Pat, [LIST("field", "fields", PatRecordField)],
   tostr = '''
   "{: " + sep(fields, ", ") + " :}"
   ''',
   extracode = [EXPLIKE]
   )


PatRange = CLS("PatRange", Pat, 
     [FIELD("low", Pat), FIELD("high", Pat)],
     tostr = ''' low + ".." + high ''',
     extracode=[EXPLIKE]
     )

PatMethodCall = CLS("PatMethodCall", Pat,
    [FIELD("methodName", Id),
     FIELD("subpat", Pat, nullable=True)
     ],
    tostr = '''
      "." + methodName + "("
      + (subpat == null ? "" : subpat.toString() )
      + ")"
    ''',
    extracode=[EXPLIKE]
    )

PatSlash = CLS("PatSlash", Pat,
   [FIELD("re", Cmd, isChild = False),
    FIELD("pat", Pat, isChild = False),
    FIELD("actualCode", Pat)
    ],
   tostr = '''
     re + "/" + pat
   ''')
################################################################
## Records
################################################################

RecordField = CLS("RecordField", Syntax, [FIELD("id", Id), FIELD("exp", Cmd)],
   tostr = ''' id + ":" + exp '''
   )

RecordCtor = CLS("RecordCtor", Cmd, [LIST("field", "fields", RecordField)],
   tostr = '''
   "\u2039" + sep(fields, ", ") + "\u203A"
   ''',
   extracode = [EXPLIKE]
   )

RecordCall = CLS("RecordCall", Cmd, [FIELD("function", Cmd), 
           LIST("field", "fields", RecordField) ],
   tostr = ''' function +  "\u2039" + sep(fields, ", ") + "\u203A" ''',
   extracode = [EXPLIKE]
   )

PostExpRecordArgs = CLS("PostExpRecordArgs", PostExp,
   [LIST("field", "fields", RecordField) ],
   tostr = ''' "\u2039" + sep(fields, ", ") + "\u203A" '''
   )


################################################################
#
# Classes and Objects
#
################################################################

# The spec separates ClassMember and ObjMember.
# Constructors are ClassMembers but not ObjMembers.
# I am not bothering with that.

MethDecl = CLS("MethDecl", Syntax, 
       [FIELD("name", Id), 
        FIELD("funbody", FunBody),
        PURITY
        ],
   implements = ANYMEMBER + [Puretic],
   tostr = ''' "def " + asStmt(funbody) ''',
   extracode = [PURETICS,
    '''
      public Id idOfName(){return name;}
      public Syntax origin; 
    '''    
   ],
   deepCopyFiddling = ['''
     ((MethDecl)copy).origin = origin;
   ''']
   )

ClsCtorDef = CLS("ClsCtorDef", Syntax, [FIELD("id", Id), FIELD("monobody", MonoBody)],
   implements = [ClassMember],
   tostr=''' "new " + monobody '''
   )

ClsPatDef = CLS("ClsPatDef", Syntax, 
   [FIELD("id", Id), LIST("formal", "formals",Id), 
    FIELD("body", Cmd)],
   implements = [ClassMember, ObjectMember],
   tostr = '''
    "pat " + id + "(" + sep(formals, ",") + ")" 
    + " = " + body + ""
   '''
   )

ClsExtends = CLS("ClsExtends", Syntax, 
   [FIELD("superName", QualName), ARGS],
   tostr = ''' superName + "(" + sep(args, ",") + ")"''',
   extracode = [ '''
     public Seal superclassSeal;
   ''']
   )

EXTENDS = LIST("ext", "exts", ClsExtends)

AnonObj = CLS("AnonObj", Cmd, 
   [LIST("ext", "exts", ClsExtends, isChild=False),
    LIST("member", "members", ObjectMember, isChild=False),
    PURITY,
    FIELD("actualCode", Cmd)
    ],
   implements = [Classlike, Puretic],
   tostr = '''
      "object" + 
      (isMarkedPure ? ":pure " : "") +
      (exts.isEmpty() ? "" : "extends " + sep(exts, ","))
      + "{"
      + sepStmt(members, " ")
      + "} "
      ''',
   extracode = [EXPLIKE, PURETICS,
   '''
     public ClassStatic classStatic;
     public SealForClass classSeal;
     public List<ClassFormal> params() {return Collections.EMPTY_LIST;}
     public boolean hasParams(){return false;}
     public Id name() {return null; }
     public List<ClassMember> members() {
        // OK, we're cheating on the type system here.
        return (List<ClassMember>) (List) this.members;
        }
     public void setClassStatic(ClassStatic cs) {
        this.classStatic = cs;
        }
     public List<ClsExtends> extendses() {return this.exts;}
     	
     public ClassStatic classStatic() {return classStatic;} 
     public SealForClass classSeal() {return classSeal;}
     public void setClassSeal(SealForClass classSeal) {
        this.classSeal = classSeal;
        }
     public Id idOfName(){return null;}
   ''']
   )


ClassFormal = CLS("ClassFormal", Cmd, 
    [FIELD("name", Id),
     FIELD("isVar", Boolean, isChild=False),
     FIELD("typeConstraints", TypeConstraints)
     ],
    tostr = ''' 
      (isVar ? "var " : "") 
      + name
      + typeConstraints
      '''
    )


     
      
ClsDecl = CLS("ClsDecl", Cmd, 
    [PURITY,
     FIELD("name", Id),
     FIELD("hasParams", Boolean, isChild=False),
     LIST("param", "params", ClassFormal),     
     EXTENDS,
     LIST("member", "members", ClassMember)
    ],
    implements = [ModuleFileMember, Classlike, Puretic, ProcMember, LocalMember ],
    tostr=''' 
      ""
      + "class " + name 
      + (params.isEmpty() ? "" : "(" + sep(params, ", ") + ")"  )
      + (isMarkedPure ? ":pure" : "") 
      + (exts.isEmpty() ? "" : " extends " + sep(exts, ","))
      + "{"
      + sepStmt(members, " ")
      + "} "     
      ''',
     extracode=[ 
     PURETICS,
     '''
       public ClassStatic classStatic;
       public SealForClass classSeal;
       public List<ClassFormal> params() {return this.params;}
       public Id name(){return this.name;};
       public boolean hasParams() {return this.hasParams;}
       public List<ClassMember> members() {return this.members;};
       public void setClassStatic(ClassStatic cs) {this.classStatic = cs;}
       public List<ClsExtends> extendses() {return this.exts;}
	
       public ClassStatic classStatic() {return this.classStatic;}
       public void setClassSeal(SealForClass classSeal) {this.classSeal = classSeal;}
       public SealForClass classSeal() {return this.classSeal;}
       public Id idOfName(){return this.name;}
        ''']
      )


#######################################################
#
# Module features
#
#######################################################

ModArgBinding = CLS("ModArgBinding", Syntax, [FIELD("formal", Id), FIELD("actual", QualName)],
    tostr = '''
    formal + "=" + actual
    '''
    )

ImportStmt = CLS("ImportStmt", Cmd, 
    [
      FIELD("isOwn", Boolean, isChild = False),
      FIELD("modName", QualName),
      LIST("modArg", "modArgs", ModArgBinding),
      FIELD("as", Id, nullable = True), 
      FIELD("dotstar", Boolean, isChild=False)
    ],
    implements = ANYMEMBER, 
    tostr = '''
      "import "
      + (isOwn ? " own " : "")  
      + (as == null ? "" : as + " = ")
      + modName
      + (dotstar ? ".*" : "")
      + (modArgs.isEmpty() ? "" : " (" + sep(modArgs, ", ") + ")")
      + ""
      + ";"
    ''',
    extracode = ['''
    public fisher.syn.importage.AbstractImport importage; 
    ''']
    )

Alias = CLS("Alias", Cmd, [FIELD("newname", QualName), FIELD("oldname", QualName)],
    tostr = '''
      "alias " + newname + " = " + oldname + ";"
      '''
    )


ModuleFileMemberStmt = CLS("ModuleFileMemberStmt", Syntax, 
    [FIELD("filename", String, isChild = False)],
    implements = [ModuleFileMember],
    tostr = ''' "member \\\"" + filename + "\\\";" '''
    )

ModuleFileImport = CLS("ModuleFileImport", Syntax,
    [FIELD("imp", ImportStmt)],
    implements = [ModuleFileMember],
    tostr = ''' imp.toString() '''
    )

ModuleFileVisibility = CLS("ModuleFileVisibility", Syntax,
    [FIELD("vis", Visibility, isChild=False), ID],
    implements = [ModuleFileMember],
    tostr = ''' vis + " " + id '''
    )

ModuleFileAlias = CLS("ModuleFileAlias", Syntax,
    [FIELD("ali", Alias)],
    implements = [ModuleFileMember],
    tostr = ''' ali.toString() '''
    )
    

Module = CLS("Module", Syntax, 
  [FIELD("isMarkedPure", Boolean, isChild=False), FIELD("name", QualName), LIST("bit", "bits", ModuleFileMember)],
  tostr = '''
     (isMarkedPure ? "pure " : "") + 
     "module " + name + "{" + sep(bits, "") + "} "
  ''',
  extracode = ['''
  public SealForModule moduleSeal;
  ''']
  )


ModulesInAList = CLS("ModulesInAList", Syntax, 
   [LIST("module", "modules", Module)],
   tostr = '''sep(modules, "\\n")''',
   extracode = "/*This is a convenience for testing.  Not to be actually used. unless useful!*/"
   )

CmdsInAList = CLS("CmdsInAList", Cmd,
   [LIST("cmd", "cmds", Cmd)],
   tostr = ''' sep(cmds, "\\n")'''
   )

SyntaxInAList = CLS("SyntaxInAList", Syntax,
   [LIST("syn", "syns", Syntax)],
   tostr = ''' sep(syns, "\\n") ''')
   

################################################################
#
# Tables
#
################################################################


# The following is an attempt to keep table items and query table items
# in synch.  However, query table items aren't exactly the same as
# non-query ones, because some query ones need initializers and
# the corresponding table ones can't have them:
#    table{key a; val b}
# vs
#    %table(key a = x.a; val b = x.b % x <- L)
# But other table items don't have this property;
# in particular, table-wide methods.
#
# Anyways, that's why TableFields have initializers here,
# even though initializers aren't allowed in tables per se.

IdWithOptInit = CLS("IdWithOptInit", Syntax,
   [FIELD("id", Id),
    FIELD("init", Cmd, nullable=True),
    FIELD("typeConstraints", TypeConstraints),
    FIELD("colAccess", ColAccess, isChild=False)],
   tostr = '''
     id 
     + typeConstraints.toString()
     + (init == null ? "" 
        : " " + colAccess.op + " " + init
       )
   '''
   )

TableFields = CLS("TableFields", Syntax, 
             [FIELD("colAccess", ColAccess, isChild=False), 
              FIELD("colSpecial", ColSpecial, isChild=False),
              LIST("idInit", "idInits", IdWithOptInit)
              ],
   implements = [TableMember], 
   tostr = '''
      colSpecial.asPrefix() + colAccess + " " + sep(idInits, ", ") + ";"
   ''' 
   )

# TablewideField = CLS("TablewideField", Syntax, 
#    [FIELD("fieldAccess", ColAccess, isChild=False), ID, FIELD("initVal", Cmd)],
#    implements = [TableMember],                
#    tostr = '''
#       "table " + fieldAccess + " " + id + " " + fieldAccess.op + " " + initVal + ";"
#    '''
#    )

AbstractTable = CLS("AbstractTable", Cmd, 
   [],
   mods=[Abstract],
   extracode = [EXPLIKE,
   '''
      public String[] colNames;
      public fisher.runtime.auxil.ColInfo[] colInfos; 
      public fisher.runtime.auxil.ColInfo[] keyInfos; 
      public fisher.runtime.auxil.ColInfo[] nonkeyInfos; 
      public fisher.runtime.auxil.ColInfo mapColInfo;
   ''']
   )      

TableKey = CLS("TableKey", Syntax, 
   [FIELD("name", Id),
    FIELD("typeConstraints", TypeConstraints)],
   tostr = '''name + "" + typeConstraints'''
   )
   

Table = CLS("Table", AbstractTable,
    [
      LIST("key", "keys", TableKey),
      LIST("val", "vals", TableFields)
    ],
    tostr = ''' 
      "table(" + sep(keys, ",") + ")" 
      + 
      "{" + sep(vals, " ") + "} "
      '''
    )


Ord = CLS("Ord", AbstractTable, 
      [LIST("field", "fields", TableFields)],
      tostr = '''
        "ord{" +  sep(fields, " ") + "} "
      '''
      )

# MapCtorKV = CLS("MapCtorKV", Syntax,
#    [FIELD("k", Cmd), FIELD("v", Cmd)],
#    tostr = ''' k + " => " + v '''
#    )

MapCtor = CLS("MapCtor", Cmd,
    [
       FIELD("actualCode", Cmd)
    ],
    tostr = ''' "map{}" '''
    )

#######################################################
#
# Queries
#
#######################################################

QueryControl = CLS("QueryControl", Syntax, [], mods=[Abstract])

QueryControlFor = CLS("QueryControlFor", QueryControl,
   [FIELD("pat", Pat), FIELD("list", Cmd),
    FIELD("inquisitive", Boolean, isChild=False)
   ],
   tostr = ''' "for " + pat +
      (inquisitive ? " <~ " : " <- ")
      + list '''
   )

QueryControlIf = CLS("QueryControlIf", QueryControl,
   [FIELD("pred", Cmd)],
   tostr = ''' "if " + pred'''
   )

QueryControlVal = CLS("QueryControlVal", QueryControl, 
   [FIELD("pat", Pat), FIELD("exp", Cmd)],
   tostr = ''' "val " + pat + " = " + exp '''
   )

QueryControlVar = CLS("QueryControlVar", QueryControl,
   [FIELD("var", Id), 
    FIELD("init", Cmd),
    FIELD("next", Cmd),
    FIELD("doBeforeFirst", Boolean, isChild=False)],
   tostr = '''
     "var " + var + " := " + init 
     + (doBeforeFirst ? " %then0 " : " %then1 ")
     + next     
   '''
   )

QueryControlWhile = CLS("QueryControlWhile", QueryControl,
   [FIELD("test", Cmd),
    FIELD("isUntil", Boolean, isChild=False)],
   tostr = ''' (isUntil ? "until " : "while ") + test ''')


QueryAbstract = CLS("QueryAbstract", Cmd, 
   [LIST("control", "controls", QueryControl, isChild=False),
    FIELD("actualCode", Cmd)
   ],
   mods=[Abstract],
   tostr = ''' 
     " | " 
     + sep(controls, ", ")
   '''
   )

QuerySwiss = CLS("QuerySwiss", QueryAbstract,
   [FIELD("one", Cmd, isChild=False), 
    FIELD("more", Cmd, nullable=True, isChild=False),
    FIELD("none", Cmd, nullable=True, isChild=False)],
   tostr = '''
     "%(" + one
     + (more == null ? "" : " %> " + more)
     + (none == null ? "" : " %< " + none)
     + super.toString()
     + ")"
   '''
   )

QueryFirstlike = CLS("QueryFirstlike", QueryAbstract,
   [FIELD("exp", Cmd, isChild=False), 
    FIELD("none", Cmd, nullable=True, isChild=False),
    FIELD("isExp", Boolean, isChild=False)
    ],
   tostr="super.toString()"
   )

QueryFirst = CLS("QueryFirst", QueryFirstlike,[],
   tostr = '''
     !isExp ? 
       (
         "first(" + sep(controls, ", ") + ")"
         +  asStmt(exp) 
         + (none == null ? "" : " else " + asStmt(none))
       )
     : (
       "%first(" + exp + 
       (none == null ? "" : " %< " + none)
       + super.toString()
       + ")"
       )
       
   '''
   )
   
QueryAfter = CLS("QueryAfter", QueryAbstract,[FIELD("exp", Cmd, isChild=False)],
   tostr = ''' "%after(" + exp + super.toString() + ")" '''
   )
   
QueryQuantifier = CLS("QueryQuantifier", QueryAbstract, 
   [FIELD("pred", Cmd, isChild=False)],
   tostr =''' 
     "(" + pred + super.toString() + ")"
   ''',
   mods=[Abstract]
   )

QueryQuantifierCount = CLS("QueryQuantifierCount", QueryQuantifier,[],
   tostr = ''' "%count" + super.toString() ''')

QueryQuantifierEvery = CLS("QueryQuantifierEvery", QueryQuantifier,[],
   tostr = ''' "%every" + super.toString() ''')

QueryQuantifierSome = CLS("QueryQuantifierSome", QueryQuantifier,[],
   tostr = ''' "%some" + super.toString() ''')                        

QueryListComprehension = CLS("QueryListComprehension", QueryAbstract,
   [FIELD("exp", Cmd, isChild=False),
   FIELD("isAppended", Boolean, isChild=False)],
   tostr='''
     "%%[" 
     + exp 
     + (isAppended ? %s : "")
     + super.toString()
     + "]"
   ''' % ELLIPSIS)

QueryTable = CLS("QueryTable", QueryAbstract,
    [
      LIST("key", "keys", IdWithOptInit, isChild=False),
      LIST("item", "items", TableFields, isChild=False)
    ],
    tostr = ''' 
      "%table(" 
      + sep(keys, ", ")
      + ")"
      + "{"
      + sep(items, " ")
      + super.toString()
      + "} "
    ''',
    extracode = [EXPLIKE]
    )

QGKey = CLS("QGKey", Syntax,
   [FIELD("id", Id),
    FIELD("init", Cmd)],
   tostr = ''' id + "=" + init '''
   )

QGAccum = CLS("QGAccum", Syntax,
   [ FIELD("colAccess", ColAccess, isChild=False),
     FIELD("colSpecial", ColSpecial, isChild = False),
     FIELD("id", Id),
     FIELD("first", Cmd),
     FIELD("then", Cmd),
     FIELD("after", Cmd)],
  tostr = '''
    colSpecial.asPrefix() + colAccess + " " 
    + id + " = " 
    + "%first " + first
    + " %then " + then
    + (after == null ? "" : " %after " + after )
    + ";"
  '''
  )

QueryGroup = CLS("QueryGroup", QueryAbstract,
    [
        LIST("key", "keys", QGKey, isChild = False),
        LIST("accum", "accums", QGAccum, isChild = False)
    ],
    tostr = ''' 
      "%group(" 
      + sep(keys, ", ")
      + ")"
      + "{"
      + sep(accums, " ")
      + super.toString()
      + "} "
    ''',
    extracode = [EXPLIKE]
    )
  

SortKey = CLS("SortKey", Syntax,
    [FIELD("sortOrder", SortOrder, isChild=False),
     FIELD("key", Cmd)],
    tostr = ''' sortOrder.op + " " + key '''
    )

QuerySort = CLS("QuerySort", QueryAbstract,
    [FIELD("exp", Cmd, isChild=False),
     LIST("sortkey", "sortkeys", SortKey, isChild=False)],
    tostr = '''
      "%sort(" + exp + " " + sep(sortkeys, " ") + super.toString() + ")"
    ''')

################################################################
#
# Procs
#
################################################################

HighLevelCommunication = CLS("HighLevelCommunication", Syntax,
   [FIELD("name", Id), FIELD("funbody", FunBody)],
   mods=[Abstract],                       
   implements = [ProcMember],
   extracode = ['''
     public Id gennedFunName;
   ''']
   )


SyncDecl = CLS("SyncDecl", HighLevelCommunication, 
   [],
   tostr = ''' "sync " + (funbody) ''',
   )

AsyncDecl = CLS("AsyncDecl", HighLevelCommunication,
   [],
   tostr = ''' "async " + (funbody) ''',
   )

# Stable
# Init
# Reinit
# Body

ProcInit = CLS("ProcInit", Syntax,
   [FIELD("initcode", Cmd)],
   implements = [ProcMember], 
   tostr = ''' "initially{" + initcode + "} " '''
   )
   
ProcBody = CLS("ProcBody", Syntax,
   [FIELD("bodycode", Cmd)],
   implements = [ProcMember], 
   tostr = ''' "body{" + bodycode + "} " '''
   )

# Though Spawn gets seriously desugared
# (viz. serve, sync, and async induce stuff.)
# But we don't use the usual actualCode approach.
# Instead, the Ingester calls the desugarer,
# after the Ingester has sorted all the bits into the
# proper buckets.  This is helpful because we want all the
# bits sorted into the proper buckets in order to do the
# desugaring.  
Spawn = CLS("Spawn", Cmd,
  [ FIELD("name", Id, nullable=True),
    LIST("bit", "bits", ProcMember)],
  implements = [ComponentInfo],
  tostr = ''' "spawn" 
      + (name == null ? "" : " " + name + "")
      + "{" + sepStmt(bits, " ") + "} " ''',
  extracode = ['''
    public List<ProcMember> bits() {return bits;}

    public List<LocalMember> localMembers = new ArrayList<LocalMember>();
    public List<HighLevelCommunication> highLevelCommunications 
      = new ArrayList<HighLevelCommunication>(); 
    public ProcBody body = null;
    public ProcInit init = null;
    public List<FunDecl> funs = new ArrayList<FunDecl>();
    
    public List<LocalMember> localMembers() { return localMembers; }
    public List<HighLevelCommunication> highLevelCommunications() {
      return highLevelCommunications;
      }
    public ProcBody body() {return body;}
    public ProcInit init() {return init;}
    public List<FunDecl> funs() {return funs; }
    public Id name() {return name;}
    public Set<Seal> freeVarSeals = new HashSet<Seal>();
    public Set<Seal> freeVarSeals() {return freeVarSeals;}

    private Id serveId = fisher.desugar.DesugarUtils.gensym(this.start, "serve");
    public Id serveId() {return serveId;}
    
  ''']
  )

SpawnByComponentName = CLS("SpawnByComponentName", Cmd,
   [FIELD("exp", Cmd), ARGS],
   tostr = ''' "spawn " + exp + "(" + sep(args, ", ") + ")" +  ";"'''
   )

#######################################################
#
# Proc-related commands
#
#######################################################

AsyncStmt = CLS("AsyncStmt", Cmd,
  [FIELD("receiver", Cmd, isChild=False),
   FIELD("id", Id, isChild=False),
   ARGS_NotChild,
   FIELD("security", Cmd, nullable=True),
   FIELD("actualCode", Cmd)
   ],
  tostr = ''' receiver + " <-- " + id + "(" + sep(args, ", ") + ")"
      + (security == null ? "" : " security " + security)
  ''',
  extracode = [EXPLIKE]
  )

SyncStmt = CLS("SyncStmt", Cmd,
  [FIELD("receiver", Cmd, isChild=False),
   FIELD("id", Id, isChild=False),
   ARGS_NotChild,
   FIELD("timeout", Cmd, nullable=True , isChild=False),
   FIELD("timeoutCmd", Cmd, nullable=True , isChild=False),
   FIELD("security", Cmd, nullable=True),
   FIELD("actualCode", Cmd)
   ],
  tostr = ''' receiver + "<->" + id + "(" + sep(args, ", ") + ")" 
      + (security == null ? "" : " security " + security)
      + (timeout == null ? "" : " timeout(" + timeout + ")" 
         + (timeoutCmd == null ? "" : timeoutCmd)
     )

     ''',
  extracode = [EXPLIKE]
  )

Send = CLS("Send", Cmd,
   [FIELD("receiver", Cmd), FIELD("exp", Cmd),
    FIELD("security", Cmd, nullable=True)],
   tostr = ''' receiver + " <<< " + exp
      + (security == null ? "" : " security " + security)
    ''',
   extracode = [EXPLIKE]
   )

Recv = CLS("Recv", Cmd,
   [LIST("acase", "cases", Case), 
    FIELD("timeoutLen", Cmd, nullable=True),
    FIELD("timeoutCmd", Cmd, nullable=True)
    ],
   tostr = ''' "recv{"  + sep(cases, " | ") + 
     (timeoutLen == null ? "" : " timeout(" + timeoutLen + ")" + timeoutCmd )+
     "} "''',
   extracode = [
     '''
        public List< List<Case> > casesByPrio;
     '''
   ],
   deepCopyFiddling = ['''
     try {
       fisher.ingest.Ingester.IT.visit((Recv)copy, null); // sort copied cases by prio
     }catch(fisher.util.FisherException fe) {
       fe.printStackTrace();
     }
   ''']
    )

ServeBlock = CLS("ServeBlock", Syntax,
   [FIELD("name", String, isChild=False), # "before" or "after"
    LIST("formal", "formals", Id),
    FIELD("cmd", Cmd)],
   tostr = '''
     name 
     + (formals.size() == 0 ? "" : "(" + sep(formals, ", ") + ")")
     + cmd        
   ''')

Serve = CLS("Serve", Cmd,
   [FIELD("before", ServeBlock, nullable = True, isChild=False),
    FIELD("after", ServeBlock, nullable = True, isChild=False),
    FIELD("timeout", Cmd, nullable = True, isChild=False),
    FIELD("timeoutCmd", Cmd, nullable = True, isChild=False),
    LIST("acase", "cases", Case, isChild=False),
    ],
   tostr = '''
     "serve"
    + (before == null ? "" : " " + before + "")
    + (after == null ? "" : " " + after + "")
    + (timeout == null ? "" : " timeout(" + timeout + ")"
       + (timeoutCmd == null ? "" : timeoutCmd.toString())
       )
    + (cases.isEmpty() ? "" : " catch {" + sep(cases, " | ") + "} ")
    + ";"
   ''',
   extracode = ['''
      public ComponentInfo componentInfo;
      public Cmd actualCode;
   ''']
   )

ComponentDecl = CLS("ComponentDecl", Cmd,
  [FIELD("name", Id,isChild=False),
   FIELD("formals", Formals, isChild=False),
   LIST("bit", "bits", ProcMember, isChild=False),
   FIELD("actualCode", Cmd)
   ],
  implements = [ModuleFileMember],
  tostr = ''' "component " +name  + formals 
    + "{" + sepStmt(bits, " ") + "} " ''',
  extracode = ['''
  public Spawn asSpawn() {
    return new Spawn(this.start, this.end, this.name, this.bits);
  }
  ''']
  )
  

Javaly = CLS("Javaly", Cmd, [], mods = [Abstract]);

JavalyFun = CLS("JavalyFun", Javaly,
  [FIELD("name", Id),
   LIST("formal", "formals", Id),
   FIELD("impl", QualName)
  ],
  implements = [ModuleFileMember],
  tostr = '''
    "javaly fun " + name + "(" + sep(formals, ",") + ") = " + impl + ";"
  ''',
  extracode = ['''
    public java.lang.reflect.Method method; 
  ''']
  )

JavalyNewDecl = CLS("JavalyNewDecl", Javaly, 
  [FIELD("name", Id),
   LIST("formal", "formals", Id)
  ],
  implements = [],
  tostr = '''
    "new " + name + "(" + sep(formals, ",") + ")" + ";"
  ''',
  extracode = ['''
    public java.lang.reflect.Method method; 
  ''']
  )                


JavalyMethodDecl = CLS("JavalyMethodDecl", Javaly, 
  [FIELD("name", Id),
   LIST("formal", "formals", Id),
   FIELD("impl", Id)
  ],
  implements = [],
  tostr = '''
    "method " + name + "(" + sep(formals, ",") + ") = " + impl + ";"
  ''',
  extracode = ['''
    public java.lang.reflect.Method method; 
  ''']
  )                


JavalyClassDecl = CLS("JavalyClassDecl", Javaly,
 [FIELD("name", Id),
  FIELD("impl", QualName),
  LIST("method", "methods", JavalyMethodDecl),
  LIST("ctor", "ctors", JavalyNewDecl),
  LIST("field", "fields", Id),
  ],
 implements = [ModuleFileMember],
 tostr = '''
   "javaly class " + name 
   + (fields == null ? "" : "(" + sep(fields, ", ") + ")")
   + " = " + impl + " {" 
   + sep(methods, " ") + sep(ctors, " ") 
   + "} "
   ''',
 extracode = ['''
   public Class cls;
 ''']
 )  

   
#######################################################
#
# And now we generate things.
#
#######################################################

for C in AllSyntaxClasses:
    C.gen(gene)
gene.genVisitors()
print "OK, generated."
