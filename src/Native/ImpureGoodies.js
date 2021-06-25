var __globalBoolState__ = false;

 // Thanks to https://www.w3.org/TR/html52/syntax.html#named-character-references
var htmlNamedEntities = JSON.parse(`{"&Aacute;": "\\u00C1", "&Aacute": "\\u00C1", "&aacute;": "\\u00E1", "&aacute": "\\u00E1", "&Abreve;": "\\u0102", "&abreve;": "\\u0103", "&ac;": "\\u223E", "&acd;": "\\u223F", "&acE;": "\\u223E\\u0333", "&Acirc;": "\\u00C2", "&Acirc": "\\u00C2", "&acirc;": "\\u00E2", "&acirc": "\\u00E2", "&acute;": "\\u00B4", "&acute": "\\u00B4", "&Acy;": "\\u0410", "&acy;": "\\u0430", "&AElig;": "\\u00C6", "&AElig": "\\u00C6", "&aelig;": "\\u00E6", "&aelig": "\\u00E6", "&af;": "\\u2061", "&Afr;": "\\uD835\\uDD04", "&afr;": "\\uD835\\uDD1E", "&Agrave;": "\\u00C0", "&Agrave": "\\u00C0", "&agrave;": "\\u00E0", "&agrave": "\\u00E0", "&alefsym;": "\\u2135", "&aleph;": "\\u2135", "&Alpha;": "\\u0391", "&alpha;": "\\u03B1", "&Amacr;": "\\u0100", "&amacr;": "\\u0101", "&amalg;": "\\u2A3F", "&amp;": "\\u0026", "&amp": "\\u0026", "&AMP;": "\\u0026", "&AMP": "\\u0026", "&andand;": "\\u2A55", "&And;": "\\u2A53", "&and;": "\\u2227", "&andd;": "\\u2A5C", "&andslope;": "\\u2A58", "&andv;": "\\u2A5A", "&ang;": "\\u2220", "&ange;": "\\u29A4", "&angle;": "\\u2220", "&angmsdaa;": "\\u29A8", "&angmsdab;": "\\u29A9", "&angmsdac;": "\\u29AA", "&angmsdad;": "\\u29AB", "&angmsdae;": "\\u29AC", "&angmsdaf;": "\\u29AD", "&angmsdag;": "\\u29AE", "&angmsdah;": "\\u29AF", "&angmsd;": "\\u2221", "&angrt;": "\\u221F", "&angrtvb;": "\\u22BE", "&angrtvbd;": "\\u299D", "&angsph;": "\\u2222", "&angst;": "\\u00C5", "&angzarr;": "\\u237C", "&Aogon;": "\\u0104", "&aogon;": "\\u0105", "&Aopf;": "\\uD835\\uDD38", "&aopf;": "\\uD835\\uDD52", "&apacir;": "\\u2A6F", "&ap;": "\\u2248", "&apE;": "\\u2A70", "&ape;": "\\u224A", "&apid;": "\\u224B", "&apos;": "\\u0027", "&ApplyFunction;": "\\u2061", "&approx;": "\\u2248", "&approxeq;": "\\u224A", "&Aring;": "\\u00C5", "&Aring": "\\u00C5", "&aring;": "\\u00E5", "&aring": "\\u00E5", "&Ascr;": "\\uD835\\uDC9C", "&ascr;": "\\uD835\\uDCB6", "&Assign;": "\\u2254", "&ast;": "\\u002A", "&asymp;": "\\u2248", "&asympeq;": "\\u224D", "&Atilde;": "\\u00C3", "&Atilde": "\\u00C3", "&atilde;": "\\u00E3", "&atilde": "\\u00E3", "&Auml;": "\\u00C4", "&Auml": "\\u00C4", "&auml;": "\\u00E4", "&auml": "\\u00E4", "&awconint;": "\\u2233", "&awint;": "\\u2A11", "&backcong;": "\\u224C", "&backepsilon;": "\\u03F6", "&backprime;": "\\u2035", "&backsim;": "\\u223D", "&backsimeq;": "\\u22CD", "&Backslash;": "\\u2216", "&Barv;": "\\u2AE7", "&barvee;": "\\u22BD", "&barwed;": "\\u2305", "&Barwed;": "\\u2306", "&barwedge;": "\\u2305", "&bbrk;": "\\u23B5", "&bbrktbrk;": "\\u23B6", "&bcong;": "\\u224C", "&Bcy;": "\\u0411", "&bcy;": "\\u0431", "&bdquo;": "\\u201E", "&becaus;": "\\u2235", "&because;": "\\u2235", "&Because;": "\\u2235", "&bemptyv;": "\\u29B0", "&bepsi;": "\\u03F6", "&bernou;": "\\u212C", "&Bernoullis;": "\\u212C", "&Beta;": "\\u0392", "&beta;": "\\u03B2", "&beth;": "\\u2136", "&between;": "\\u226C", "&Bfr;": "\\uD835\\uDD05", "&bfr;": "\\uD835\\uDD1F", "&bigcap;": "\\u22C2", "&bigcirc;": "\\u25EF", "&bigcup;": "\\u22C3", "&bigodot;": "\\u2A00", "&bigoplus;": "\\u2A01", "&bigotimes;": "\\u2A02", "&bigsqcup;": "\\u2A06", "&bigstar;": "\\u2605", "&bigtriangledown;": "\\u25BD", "&bigtriangleup;": "\\u25B3", "&biguplus;": "\\u2A04", "&bigvee;": "\\u22C1", "&bigwedge;": "\\u22C0", "&bkarow;": "\\u290D", "&blacklozenge;": "\\u29EB", "&blacksquare;": "\\u25AA", "&blacktriangle;": "\\u25B4", "&blacktriangledown;": "\\u25BE", "&blacktriangleleft;": "\\u25C2", "&blacktriangleright;": "\\u25B8", "&blank;": "\\u2423", "&blk12;": "\\u2592", "&blk14;": "\\u2591", "&blk34;": "\\u2593", "&block;": "\\u2588", "&bne;": "\\u003D\\u20E5", "&bnequiv;": "\\u2261\\u20E5", "&bNot;": "\\u2AED", "&bnot;": "\\u2310", "&Bopf;": "\\uD835\\uDD39", "&bopf;": "\\uD835\\uDD53", "&bot;": "\\u22A5", "&bottom;": "\\u22A5", "&bowtie;": "\\u22C8", "&boxbox;": "\\u29C9", "&boxdl;": "\\u2510", "&boxdL;": "\\u2555", "&boxDl;": "\\u2556", "&boxDL;": "\\u2557", "&boxdr;": "\\u250C", "&boxdR;": "\\u2552", "&boxDr;": "\\u2553", "&boxDR;": "\\u2554", "&boxh;": "\\u2500", "&boxH;": "\\u2550", "&boxhd;": "\\u252C", "&boxHd;": "\\u2564", "&boxhD;": "\\u2565", "&boxHD;": "\\u2566", "&boxhu;": "\\u2534", "&boxHu;": "\\u2567", "&boxhU;": "\\u2568", "&boxHU;": "\\u2569", "&boxminus;": "\\u229F", "&boxplus;": "\\u229E", "&boxtimes;": "\\u22A0", "&boxul;": "\\u2518", "&boxuL;": "\\u255B", "&boxUl;": "\\u255C", "&boxUL;": "\\u255D", "&boxur;": "\\u2514", "&boxuR;": "\\u2558", "&boxUr;": "\\u2559", "&boxUR;": "\\u255A", "&boxv;": "\\u2502", "&boxV;": "\\u2551", "&boxvh;": "\\u253C", "&boxvH;": "\\u256A", "&boxVh;": "\\u256B", "&boxVH;": "\\u256C", "&boxvl;": "\\u2524", "&boxvL;": "\\u2561", "&boxVl;": "\\u2562", "&boxVL;": "\\u2563", "&boxvr;": "\\u251C", "&boxvR;": "\\u255E", "&boxVr;": "\\u255F", "&boxVR;": "\\u2560", "&bprime;": "\\u2035", "&breve;": "\\u02D8", "&Breve;": "\\u02D8", "&brvbar;": "\\u00A6", "&brvbar": "\\u00A6", "&bscr;": "\\uD835\\uDCB7", "&Bscr;": "\\u212C", "&bsemi;": "\\u204F", "&bsim;": "\\u223D", "&bsime;": "\\u22CD", "&bsolb;": "\\u29C5", "&bsol;": "\\u005C", "&bsolhsub;": "\\u27C8", "&bull;": "\\u2022", "&bullet;": "\\u2022", "&bump;": "\\u224E", "&bumpE;": "\\u2AAE", "&bumpe;": "\\u224F", "&Bumpeq;": "\\u224E", "&bumpeq;": "\\u224F", "&Cacute;": "\\u0106", "&cacute;": "\\u0107", "&capand;": "\\u2A44", "&capbrcup;": "\\u2A49", "&capcap;": "\\u2A4B", "&cap;": "\\u2229", "&Cap;": "\\u22D2", "&capcup;": "\\u2A47", "&capdot;": "\\u2A40", "&CapitalDifferentialD;": "\\u2145", "&caps;": "\\u2229\\uFE00", "&caret;": "\\u2041", "&caron;": "\\u02C7", "&Cayleys;": "\\u212D", "&ccaps;": "\\u2A4D", "&Ccaron;": "\\u010C", "&ccaron;": "\\u010D", "&Ccedil;": "\\u00C7", "&Ccedil": "\\u00C7", "&ccedil;": "\\u00E7", "&ccedil": "\\u00E7", "&Ccirc;": "\\u0108", "&ccirc;": "\\u0109", "&Cconint;": "\\u2230", "&ccups;": "\\u2A4C", "&ccupssm;": "\\u2A50", "&Cdot;": "\\u010A", "&cdot;": "\\u010B", "&cedil;": "\\u00B8", "&cedil": "\\u00B8", "&Cedilla;": "\\u00B8", "&cemptyv;": "\\u29B2", "&cent;": "\\u00A2", "&cent": "\\u00A2", "&centerdot;": "\\u00B7", "&CenterDot;": "\\u00B7", "&cfr;": "\\uD835\\uDD20", "&Cfr;": "\\u212D", "&CHcy;": "\\u0427", "&chcy;": "\\u0447", "&check;": "\\u2713", "&checkmark;": "\\u2713", "&Chi;": "\\u03A7", "&chi;": "\\u03C7", "&circ;": "\\u02C6", "&circeq;": "\\u2257", "&circlearrowleft;": "\\u21BA", "&circlearrowright;": "\\u21BB", "&circledast;": "\\u229B", "&circledcirc;": "\\u229A", "&circleddash;": "\\u229D", "&CircleDot;": "\\u2299", "&circledR;": "\\u00AE", "&circledS;": "\\u24C8", "&CircleMinus;": "\\u2296", "&CirclePlus;": "\\u2295", "&CircleTimes;": "\\u2297", "&cir;": "\\u25CB", "&cirE;": "\\u29C3", "&cire;": "\\u2257", "&cirfnint;": "\\u2A10", "&cirmid;": "\\u2AEF", "&cirscir;": "\\u29C2", "&ClockwiseContourIntegral;": "\\u2232", "&CloseCurlyDoubleQuote;": "\\u201D", "&CloseCurlyQuote;": "\\u2019", "&clubs;": "\\u2663", "&clubsuit;": "\\u2663", "&colon;": "\\u003A", "&Colon;": "\\u2237", "&Colone;": "\\u2A74", "&colone;": "\\u2254", "&coloneq;": "\\u2254", "&comma;": "\\u002C", "&commat;": "\\u0040", "&comp;": "\\u2201", "&compfn;": "\\u2218", "&complement;": "\\u2201", "&complexes;": "\\u2102", "&cong;": "\\u2245", "&congdot;": "\\u2A6D", "&Congruent;": "\\u2261", "&conint;": "\\u222E", "&Conint;": "\\u222F", "&ContourIntegral;": "\\u222E", "&copf;": "\\uD835\\uDD54", "&Copf;": "\\u2102", "&coprod;": "\\u2210", "&Coproduct;": "\\u2210", "&copy;": "\\u00A9", "&copy": "\\u00A9", "&COPY;": "\\u00A9", "&COPY": "\\u00A9", "&copysr;": "\\u2117", "&CounterClockwiseContourIntegral;": "\\u2233", "&crarr;": "\\u21B5", "&cross;": "\\u2717", "&Cross;": "\\u2A2F", "&Cscr;": "\\uD835\\uDC9E", "&cscr;": "\\uD835\\uDCB8", "&csub;": "\\u2ACF", "&csube;": "\\u2AD1", "&csup;": "\\u2AD0", "&csupe;": "\\u2AD2", "&ctdot;": "\\u22EF", "&cudarrl;": "\\u2938", "&cudarrr;": "\\u2935", "&cuepr;": "\\u22DE", "&cuesc;": "\\u22DF", "&cularr;": "\\u21B6", "&cularrp;": "\\u293D", "&cupbrcap;": "\\u2A48", "&cupcap;": "\\u2A46", "&CupCap;": "\\u224D", "&cup;": "\\u222A", "&Cup;": "\\u22D3", "&cupcup;": "\\u2A4A", "&cupdot;": "\\u228D", "&cupor;": "\\u2A45", "&cups;": "\\u222A\\uFE00", "&curarr;": "\\u21B7", "&curarrm;": "\\u293C", "&curlyeqprec;": "\\u22DE", "&curlyeqsucc;": "\\u22DF", "&curlyvee;": "\\u22CE", "&curlywedge;": "\\u22CF", "&curren;": "\\u00A4", "&curren": "\\u00A4", "&curvearrowleft;": "\\u21B6", "&curvearrowright;": "\\u21B7", "&cuvee;": "\\u22CE", "&cuwed;": "\\u22CF", "&cwconint;": "\\u2232", "&cwint;": "\\u2231", "&cylcty;": "\\u232D", "&dagger;": "\\u2020", "&Dagger;": "\\u2021", "&daleth;": "\\u2138", "&darr;": "\\u2193", "&Darr;": "\\u21A1", "&dArr;": "\\u21D3", "&dash;": "\\u2010", "&Dashv;": "\\u2AE4", "&dashv;": "\\u22A3", "&dbkarow;": "\\u290F", "&dblac;": "\\u02DD", "&Dcaron;": "\\u010E", "&dcaron;": "\\u010F", "&Dcy;": "\\u0414", "&dcy;": "\\u0434", "&ddagger;": "\\u2021", "&ddarr;": "\\u21CA", "&DD;": "\\u2145", "&dd;": "\\u2146", "&DDotrahd;": "\\u2911", "&ddotseq;": "\\u2A77", "&deg;": "\\u00B0", "&deg": "\\u00B0", "&Del;": "\\u2207", "&Delta;": "\\u0394", "&delta;": "\\u03B4", "&demptyv;": "\\u29B1", "&dfisht;": "\\u297F", "&Dfr;": "\\uD835\\uDD07", "&dfr;": "\\uD835\\uDD21", "&dHar;": "\\u2965", "&dharl;": "\\u21C3", "&dharr;": "\\u21C2", "&DiacriticalAcute;": "\\u00B4", "&DiacriticalDot;": "\\u02D9", "&DiacriticalDoubleAcute;": "\\u02DD", "&DiacriticalGrave;": "\\u0060", "&DiacriticalTilde;": "\\u02DC", "&diam;": "\\u22C4", "&diamond;": "\\u22C4", "&Diamond;": "\\u22C4", "&diamondsuit;": "\\u2666", "&diams;": "\\u2666", "&die;": "\\u00A8", "&DifferentialD;": "\\u2146", "&digamma;": "\\u03DD", "&disin;": "\\u22F2", "&div;": "\\u00F7", "&divide;": "\\u00F7", "&divide": "\\u00F7", "&divideontimes;": "\\u22C7", "&divonx;": "\\u22C7", "&DJcy;": "\\u0402", "&djcy;": "\\u0452", "&dlcorn;": "\\u231E", "&dlcrop;": "\\u230D", "&dollar;": "\\u0024", "&Dopf;": "\\uD835\\uDD3B", "&dopf;": "\\uD835\\uDD55", "&Dot;": "\\u00A8", "&dot;": "\\u02D9", "&DotDot;": "\\u20DC", "&doteq;": "\\u2250", "&doteqdot;": "\\u2251", "&DotEqual;": "\\u2250", "&dotminus;": "\\u2238", "&dotplus;": "\\u2214", "&dotsquare;": "\\u22A1", "&doublebarwedge;": "\\u2306", "&DoubleContourIntegral;": "\\u222F", "&DoubleDot;": "\\u00A8", "&DoubleDownArrow;": "\\u21D3", "&DoubleLeftArrow;": "\\u21D0", "&DoubleLeftRightArrow;": "\\u21D4", "&DoubleLeftTee;": "\\u2AE4", "&DoubleLongLeftArrow;": "\\u27F8", "&DoubleLongLeftRightArrow;": "\\u27FA", "&DoubleLongRightArrow;": "\\u27F9", "&DoubleRightArrow;": "\\u21D2", "&DoubleRightTee;": "\\u22A8", "&DoubleUpArrow;": "\\u21D1", "&DoubleUpDownArrow;": "\\u21D5", "&DoubleVerticalBar;": "\\u2225", "&DownArrowBar;": "\\u2913", "&downarrow;": "\\u2193", "&DownArrow;": "\\u2193", "&Downarrow;": "\\u21D3", "&DownArrowUpArrow;": "\\u21F5", "&DownBreve;": "\\u0311", "&downdownarrows;": "\\u21CA", "&downharpoonleft;": "\\u21C3", "&downharpoonright;": "\\u21C2", "&DownLeftRightVector;": "\\u2950", "&DownLeftTeeVector;": "\\u295E", "&DownLeftVectorBar;": "\\u2956", "&DownLeftVector;": "\\u21BD", "&DownRightTeeVector;": "\\u295F", "&DownRightVectorBar;": "\\u2957", "&DownRightVector;": "\\u21C1", "&DownTeeArrow;": "\\u21A7", "&DownTee;": "\\u22A4", "&drbkarow;": "\\u2910", "&drcorn;": "\\u231F", "&drcrop;": "\\u230C", "&Dscr;": "\\uD835\\uDC9F", "&dscr;": "\\uD835\\uDCB9", "&DScy;": "\\u0405", "&dscy;": "\\u0455", "&dsol;": "\\u29F6", "&Dstrok;": "\\u0110", "&dstrok;": "\\u0111", "&dtdot;": "\\u22F1", "&dtri;": "\\u25BF", "&dtrif;": "\\u25BE", "&duarr;": "\\u21F5", "&duhar;": "\\u296F", "&dwangle;": "\\u29A6", "&DZcy;": "\\u040F", "&dzcy;": "\\u045F", "&dzigrarr;": "\\u27FF", "&Eacute;": "\\u00C9", "&Eacute": "\\u00C9", "&eacute;": "\\u00E9", "&eacute": "\\u00E9", "&easter;": "\\u2A6E", "&Ecaron;": "\\u011A", "&ecaron;": "\\u011B", "&Ecirc;": "\\u00CA", "&Ecirc": "\\u00CA", "&ecirc;": "\\u00EA", "&ecirc": "\\u00EA", "&ecir;": "\\u2256", "&ecolon;": "\\u2255", "&Ecy;": "\\u042D", "&ecy;": "\\u044D", "&eDDot;": "\\u2A77", "&Edot;": "\\u0116", "&edot;": "\\u0117", "&eDot;": "\\u2251", "&ee;": "\\u2147", "&efDot;": "\\u2252", "&Efr;": "\\uD835\\uDD08", "&efr;": "\\uD835\\uDD22", "&eg;": "\\u2A9A", "&Egrave;": "\\u00C8", "&Egrave": "\\u00C8", "&egrave;": "\\u00E8", "&egrave": "\\u00E8", "&egs;": "\\u2A96", "&egsdot;": "\\u2A98", "&el;": "\\u2A99", "&Element;": "\\u2208", "&elinters;": "\\u23E7", "&ell;": "\\u2113", "&els;": "\\u2A95", "&elsdot;": "\\u2A97", "&Emacr;": "\\u0112", "&emacr;": "\\u0113", "&empty;": "\\u2205", "&emptyset;": "\\u2205", "&EmptySmallSquare;": "\\u25FB", "&emptyv;": "\\u2205", "&EmptyVerySmallSquare;": "\\u25AB", "&emsp13;": "\\u2004", "&emsp14;": "\\u2005", "&emsp;": "\\u2003", "&ENG;": "\\u014A", "&eng;": "\\u014B", "&ensp;": "\\u2002", "&Eogon;": "\\u0118", "&eogon;": "\\u0119", "&Eopf;": "\\uD835\\uDD3C", "&eopf;": "\\uD835\\uDD56", "&epar;": "\\u22D5", "&eparsl;": "\\u29E3", "&eplus;": "\\u2A71", "&epsi;": "\\u03B5", "&Epsilon;": "\\u0395", "&epsilon;": "\\u03B5", "&epsiv;": "\\u03F5", "&eqcirc;": "\\u2256", "&eqcolon;": "\\u2255", "&eqsim;": "\\u2242", "&eqslantgtr;": "\\u2A96", "&eqslantless;": "\\u2A95", "&Equal;": "\\u2A75", "&equals;": "\\u003D", "&EqualTilde;": "\\u2242", "&equest;": "\\u225F", "&Equilibrium;": "\\u21CC", "&equiv;": "\\u2261", "&equivDD;": "\\u2A78", "&eqvparsl;": "\\u29E5", "&erarr;": "\\u2971", "&erDot;": "\\u2253", "&escr;": "\\u212F", "&Escr;": "\\u2130", "&esdot;": "\\u2250", "&Esim;": "\\u2A73", "&esim;": "\\u2242", "&Eta;": "\\u0397", "&eta;": "\\u03B7", "&ETH;": "\\u00D0", "&ETH": "\\u00D0", "&eth;": "\\u00F0", "&eth": "\\u00F0", "&Euml;": "\\u00CB", "&Euml": "\\u00CB", "&euml;": "\\u00EB", "&euml": "\\u00EB", "&euro;": "\\u20AC", "&excl;": "\\u0021", "&exist;": "\\u2203", "&Exists;": "\\u2203", "&expectation;": "\\u2130", "&exponentiale;": "\\u2147", "&ExponentialE;": "\\u2147", "&fallingdotseq;": "\\u2252", "&Fcy;": "\\u0424", "&fcy;": "\\u0444", "&female;": "\\u2640", "&ffilig;": "\\uFB03", "&fflig;": "\\uFB00", "&ffllig;": "\\uFB04", "&Ffr;": "\\uD835\\uDD09", "&ffr;": "\\uD835\\uDD23", "&filig;": "\\uFB01", "&FilledSmallSquare;": "\\u25FC", "&FilledVerySmallSquare;": "\\u25AA", "&fjlig;": "\\u0066\\u006A", "&flat;": "\\u266D", "&fllig;": "\\uFB02", "&fltns;": "\\u25B1", "&fnof;": "\\u0192", "&Fopf;": "\\uD835\\uDD3D", "&fopf;": "\\uD835\\uDD57", "&forall;": "\\u2200", "&ForAll;": "\\u2200", "&fork;": "\\u22D4", "&forkv;": "\\u2AD9", "&Fouriertrf;": "\\u2131", "&fpartint;": "\\u2A0D", "&frac12;": "\\u00BD", "&frac12": "\\u00BD", "&frac13;": "\\u2153", "&frac14;": "\\u00BC", "&frac14": "\\u00BC", "&frac15;": "\\u2155", "&frac16;": "\\u2159", "&frac18;": "\\u215B", "&frac23;": "\\u2154", "&frac25;": "\\u2156", "&frac34;": "\\u00BE", "&frac34": "\\u00BE", "&frac35;": "\\u2157", "&frac38;": "\\u215C", "&frac45;": "\\u2158", "&frac56;": "\\u215A", "&frac58;": "\\u215D", "&frac78;": "\\u215E", "&frasl;": "\\u2044", "&frown;": "\\u2322", "&fscr;": "\\uD835\\uDCBB", "&Fscr;": "\\u2131", "&gacute;": "\\u01F5", "&Gamma;": "\\u0393", "&gamma;": "\\u03B3", "&Gammad;": "\\u03DC", "&gammad;": "\\u03DD", "&gap;": "\\u2A86", "&Gbreve;": "\\u011E", "&gbreve;": "\\u011F", "&Gcedil;": "\\u0122", "&Gcirc;": "\\u011C", "&gcirc;": "\\u011D", "&Gcy;": "\\u0413", "&gcy;": "\\u0433", "&Gdot;": "\\u0120", "&gdot;": "\\u0121", "&ge;": "\\u2265", "&gE;": "\\u2267", "&gEl;": "\\u2A8C", "&gel;": "\\u22DB", "&geq;": "\\u2265", "&geqq;": "\\u2267", "&geqslant;": "\\u2A7E", "&gescc;": "\\u2AA9", "&ges;": "\\u2A7E", "&gesdot;": "\\u2A80", "&gesdoto;": "\\u2A82", "&gesdotol;": "\\u2A84", "&gesl;": "\\u22DB\\uFE00", "&gesles;": "\\u2A94", "&Gfr;": "\\uD835\\uDD0A", "&gfr;": "\\uD835\\uDD24", "&gg;": "\\u226B", "&Gg;": "\\u22D9", "&ggg;": "\\u22D9", "&gimel;": "\\u2137", "&GJcy;": "\\u0403", "&gjcy;": "\\u0453", "&gla;": "\\u2AA5", "&gl;": "\\u2277", "&glE;": "\\u2A92", "&glj;": "\\u2AA4", "&gnap;": "\\u2A8A", "&gnapprox;": "\\u2A8A", "&gne;": "\\u2A88", "&gnE;": "\\u2269", "&gneq;": "\\u2A88", "&gneqq;": "\\u2269", "&gnsim;": "\\u22E7", "&Gopf;": "\\uD835\\uDD3E", "&gopf;": "\\uD835\\uDD58", "&grave;": "\\u0060", "&GreaterEqual;": "\\u2265", "&GreaterEqualLess;": "\\u22DB", "&GreaterFullEqual;": "\\u2267", "&GreaterGreater;": "\\u2AA2", "&GreaterLess;": "\\u2277", "&GreaterSlantEqual;": "\\u2A7E", "&GreaterTilde;": "\\u2273", "&Gscr;": "\\uD835\\uDCA2", "&gscr;": "\\u210A", "&gsim;": "\\u2273", "&gsime;": "\\u2A8E", "&gsiml;": "\\u2A90", "&gtcc;": "\\u2AA7", "&gtcir;": "\\u2A7A", "&gt;": "\\u003E", "&gt": "\\u003E", "&GT;": "\\u003E", "&GT": "\\u003E", "&Gt;": "\\u226B", "&gtdot;": "\\u22D7", "&gtlPar;": "\\u2995", "&gtquest;": "\\u2A7C", "&gtrapprox;": "\\u2A86", "&gtrarr;": "\\u2978", "&gtrdot;": "\\u22D7", "&gtreqless;": "\\u22DB", "&gtreqqless;": "\\u2A8C", "&gtrless;": "\\u2277", "&gtrsim;": "\\u2273", "&gvertneqq;": "\\u2269\\uFE00", "&gvnE;": "\\u2269\\uFE00", "&Hacek;": "\\u02C7", "&hairsp;": "\\u200A", "&half;": "\\u00BD", "&hamilt;": "\\u210B", "&HARDcy;": "\\u042A", "&hardcy;": "\\u044A", "&harrcir;": "\\u2948", "&harr;": "\\u2194", "&hArr;": "\\u21D4", "&harrw;": "\\u21AD", "&Hat;": "\\u005E", "&hbar;": "\\u210F", "&Hcirc;": "\\u0124", "&hcirc;": "\\u0125", "&hearts;": "\\u2665", "&heartsuit;": "\\u2665", "&hellip;": "\\u2026", "&hercon;": "\\u22B9", "&hfr;": "\\uD835\\uDD25", "&Hfr;": "\\u210C", "&HilbertSpace;": "\\u210B", "&hksearow;": "\\u2925", "&hkswarow;": "\\u2926", "&hoarr;": "\\u21FF", "&homtht;": "\\u223B", "&hookleftarrow;": "\\u21A9", "&hookrightarrow;": "\\u21AA", "&hopf;": "\\uD835\\uDD59", "&Hopf;": "\\u210D", "&horbar;": "\\u2015", "&HorizontalLine;": "\\u2500", "&hscr;": "\\uD835\\uDCBD", "&Hscr;": "\\u210B", "&hslash;": "\\u210F", "&Hstrok;": "\\u0126", "&hstrok;": "\\u0127", "&HumpDownHump;": "\\u224E", "&HumpEqual;": "\\u224F", "&hybull;": "\\u2043", "&hyphen;": "\\u2010", "&Iacute;": "\\u00CD", "&Iacute": "\\u00CD", "&iacute;": "\\u00ED", "&iacute": "\\u00ED", "&ic;": "\\u2063", "&Icirc;": "\\u00CE", "&Icirc": "\\u00CE", "&icirc;": "\\u00EE", "&icirc": "\\u00EE", "&Icy;": "\\u0418", "&icy;": "\\u0438", "&Idot;": "\\u0130", "&IEcy;": "\\u0415", "&iecy;": "\\u0435", "&iexcl;": "\\u00A1", "&iexcl": "\\u00A1", "&iff;": "\\u21D4", "&ifr;": "\\uD835\\uDD26", "&Ifr;": "\\u2111", "&Igrave;": "\\u00CC", "&Igrave": "\\u00CC", "&igrave;": "\\u00EC", "&igrave": "\\u00EC", "&ii;": "\\u2148", "&iiiint;": "\\u2A0C", "&iiint;": "\\u222D", "&iinfin;": "\\u29DC", "&iiota;": "\\u2129", "&IJlig;": "\\u0132", "&ijlig;": "\\u0133", "&Imacr;": "\\u012A", "&imacr;": "\\u012B", "&image;": "\\u2111", "&ImaginaryI;": "\\u2148", "&imagline;": "\\u2110", "&imagpart;": "\\u2111", "&imath;": "\\u0131", "&Im;": "\\u2111", "&imof;": "\\u22B7", "&imped;": "\\u01B5", "&Implies;": "\\u21D2", "&incare;": "\\u2105", "&in;": "\\u2208", "&infin;": "\\u221E", "&infintie;": "\\u29DD", "&inodot;": "\\u0131", "&intcal;": "\\u22BA", "&int;": "\\u222B", "&Int;": "\\u222C", "&integers;": "\\u2124", "&Integral;": "\\u222B", "&intercal;": "\\u22BA", "&Intersection;": "\\u22C2", "&intlarhk;": "\\u2A17", "&intprod;": "\\u2A3C", "&InvisibleComma;": "\\u2063", "&InvisibleTimes;": "\\u2062", "&IOcy;": "\\u0401", "&iocy;": "\\u0451", "&Iogon;": "\\u012E", "&iogon;": "\\u012F", "&Iopf;": "\\uD835\\uDD40", "&iopf;": "\\uD835\\uDD5A", "&Iota;": "\\u0399", "&iota;": "\\u03B9", "&iprod;": "\\u2A3C", "&iquest;": "\\u00BF", "&iquest": "\\u00BF", "&iscr;": "\\uD835\\uDCBE", "&Iscr;": "\\u2110", "&isin;": "\\u2208", "&isindot;": "\\u22F5", "&isinE;": "\\u22F9", "&isins;": "\\u22F4", "&isinsv;": "\\u22F3", "&isinv;": "\\u2208", "&it;": "\\u2062", "&Itilde;": "\\u0128", "&itilde;": "\\u0129", "&Iukcy;": "\\u0406", "&iukcy;": "\\u0456", "&Iuml;": "\\u00CF", "&Iuml": "\\u00CF", "&iuml;": "\\u00EF", "&iuml": "\\u00EF", "&Jcirc;": "\\u0134", "&jcirc;": "\\u0135", "&Jcy;": "\\u0419", "&jcy;": "\\u0439", "&Jfr;": "\\uD835\\uDD0D", "&jfr;": "\\uD835\\uDD27", "&jmath;": "\\u0237", "&Jopf;": "\\uD835\\uDD41", "&jopf;": "\\uD835\\uDD5B", "&Jscr;": "\\uD835\\uDCA5", "&jscr;": "\\uD835\\uDCBF", "&Jsercy;": "\\u0408", "&jsercy;": "\\u0458", "&Jukcy;": "\\u0404", "&jukcy;": "\\u0454", "&Kappa;": "\\u039A", "&kappa;": "\\u03BA", "&kappav;": "\\u03F0", "&Kcedil;": "\\u0136", "&kcedil;": "\\u0137", "&Kcy;": "\\u041A", "&kcy;": "\\u043A", "&Kfr;": "\\uD835\\uDD0E", "&kfr;": "\\uD835\\uDD28", "&kgreen;": "\\u0138", "&KHcy;": "\\u0425", "&khcy;": "\\u0445", "&KJcy;": "\\u040C", "&kjcy;": "\\u045C", "&Kopf;": "\\uD835\\uDD42", "&kopf;": "\\uD835\\uDD5C", "&Kscr;": "\\uD835\\uDCA6", "&kscr;": "\\uD835\\uDCC0", "&lAarr;": "\\u21DA", "&Lacute;": "\\u0139", "&lacute;": "\\u013A", "&laemptyv;": "\\u29B4", "&lagran;": "\\u2112", "&Lambda;": "\\u039B", "&lambda;": "\\u03BB", "&lang;": "\\u27E8", "&Lang;": "\\u27EA", "&langd;": "\\u2991", "&langle;": "\\u27E8", "&lap;": "\\u2A85", "&Laplacetrf;": "\\u2112", "&laquo;": "\\u00AB", "&laquo": "\\u00AB", "&larrb;": "\\u21E4", "&larrbfs;": "\\u291F", "&larr;": "\\u2190", "&Larr;": "\\u219E", "&lArr;": "\\u21D0", "&larrfs;": "\\u291D", "&larrhk;": "\\u21A9", "&larrlp;": "\\u21AB", "&larrpl;": "\\u2939", "&larrsim;": "\\u2973", "&larrtl;": "\\u21A2", "&latail;": "\\u2919", "&lAtail;": "\\u291B", "&lat;": "\\u2AAB", "&late;": "\\u2AAD", "&lates;": "\\u2AAD\\uFE00", "&lbarr;": "\\u290C", "&lBarr;": "\\u290E", "&lbbrk;": "\\u2772", "&lbrace;": "\\u007B", "&lbrack;": "\\u005B", "&lbrke;": "\\u298B", "&lbrksld;": "\\u298F", "&lbrkslu;": "\\u298D", "&Lcaron;": "\\u013D", "&lcaron;": "\\u013E", "&Lcedil;": "\\u013B", "&lcedil;": "\\u013C", "&lceil;": "\\u2308", "&lcub;": "\\u007B", "&Lcy;": "\\u041B", "&lcy;": "\\u043B", "&ldca;": "\\u2936", "&ldquo;": "\\u201C", "&ldquor;": "\\u201E", "&ldrdhar;": "\\u2967", "&ldrushar;": "\\u294B", "&ldsh;": "\\u21B2", "&le;": "\\u2264", "&lE;": "\\u2266", "&LeftAngleBracket;": "\\u27E8", "&LeftArrowBar;": "\\u21E4", "&leftarrow;": "\\u2190", "&LeftArrow;": "\\u2190", "&Leftarrow;": "\\u21D0", "&LeftArrowRightArrow;": "\\u21C6", "&leftarrowtail;": "\\u21A2", "&LeftCeiling;": "\\u2308", "&LeftDoubleBracket;": "\\u27E6", "&LeftDownTeeVector;": "\\u2961", "&LeftDownVectorBar;": "\\u2959", "&LeftDownVector;": "\\u21C3", "&LeftFloor;": "\\u230A", "&leftharpoondown;": "\\u21BD", "&leftharpoonup;": "\\u21BC", "&leftleftarrows;": "\\u21C7", "&leftrightarrow;": "\\u2194", "&LeftRightArrow;": "\\u2194", "&Leftrightarrow;": "\\u21D4", "&leftrightarrows;": "\\u21C6", "&leftrightharpoons;": "\\u21CB", "&leftrightsquigarrow;": "\\u21AD", "&LeftRightVector;": "\\u294E", "&LeftTeeArrow;": "\\u21A4", "&LeftTee;": "\\u22A3", "&LeftTeeVector;": "\\u295A", "&leftthreetimes;": "\\u22CB", "&LeftTriangleBar;": "\\u29CF", "&LeftTriangle;": "\\u22B2", "&LeftTriangleEqual;": "\\u22B4", "&LeftUpDownVector;": "\\u2951", "&LeftUpTeeVector;": "\\u2960", "&LeftUpVectorBar;": "\\u2958", "&LeftUpVector;": "\\u21BF", "&LeftVectorBar;": "\\u2952", "&LeftVector;": "\\u21BC", "&lEg;": "\\u2A8B", "&leg;": "\\u22DA", "&leq;": "\\u2264", "&leqq;": "\\u2266", "&leqslant;": "\\u2A7D", "&lescc;": "\\u2AA8", "&les;": "\\u2A7D", "&lesdot;": "\\u2A7F", "&lesdoto;": "\\u2A81", "&lesdotor;": "\\u2A83", "&lesg;": "\\u22DA\\uFE00", "&lesges;": "\\u2A93", "&lessapprox;": "\\u2A85", "&lessdot;": "\\u22D6", "&lesseqgtr;": "\\u22DA", "&lesseqqgtr;": "\\u2A8B", "&LessEqualGreater;": "\\u22DA", "&LessFullEqual;": "\\u2266", "&LessGreater;": "\\u2276", "&lessgtr;": "\\u2276", "&LessLess;": "\\u2AA1", "&lesssim;": "\\u2272", "&LessSlantEqual;": "\\u2A7D", "&LessTilde;": "\\u2272", "&lfisht;": "\\u297C", "&lfloor;": "\\u230A", "&Lfr;": "\\uD835\\uDD0F", "&lfr;": "\\uD835\\uDD29", "&lg;": "\\u2276", "&lgE;": "\\u2A91", "&lHar;": "\\u2962", "&lhard;": "\\u21BD", "&lharu;": "\\u21BC", "&lharul;": "\\u296A", "&lhblk;": "\\u2584", "&LJcy;": "\\u0409", "&ljcy;": "\\u0459", "&llarr;": "\\u21C7", "&ll;": "\\u226A", "&Ll;": "\\u22D8", "&llcorner;": "\\u231E", "&Lleftarrow;": "\\u21DA", "&llhard;": "\\u296B", "&lltri;": "\\u25FA", "&Lmidot;": "\\u013F", "&lmidot;": "\\u0140", "&lmoustache;": "\\u23B0", "&lmoust;": "\\u23B0", "&lnap;": "\\u2A89", "&lnapprox;": "\\u2A89", "&lne;": "\\u2A87", "&lnE;": "\\u2268", "&lneq;": "\\u2A87", "&lneqq;": "\\u2268", "&lnsim;": "\\u22E6", "&loang;": "\\u27EC", "&loarr;": "\\u21FD", "&lobrk;": "\\u27E6", "&longleftarrow;": "\\u27F5", "&LongLeftArrow;": "\\u27F5", "&Longleftarrow;": "\\u27F8", "&longleftrightarrow;": "\\u27F7", "&LongLeftRightArrow;": "\\u27F7", "&Longleftrightarrow;": "\\u27FA", "&longmapsto;": "\\u27FC", "&longrightarrow;": "\\u27F6", "&LongRightArrow;": "\\u27F6", "&Longrightarrow;": "\\u27F9", "&looparrowleft;": "\\u21AB", "&looparrowright;": "\\u21AC", "&lopar;": "\\u2985", "&Lopf;": "\\uD835\\uDD43", "&lopf;": "\\uD835\\uDD5D", "&loplus;": "\\u2A2D", "&lotimes;": "\\u2A34", "&lowast;": "\\u2217", "&lowbar;": "\\u005F", "&LowerLeftArrow;": "\\u2199", "&LowerRightArrow;": "\\u2198", "&loz;": "\\u25CA", "&lozenge;": "\\u25CA", "&lozf;": "\\u29EB", "&lpar;": "\\u0028", "&lparlt;": "\\u2993", "&lrarr;": "\\u21C6", "&lrcorner;": "\\u231F", "&lrhar;": "\\u21CB", "&lrhard;": "\\u296D", "&lrm;": "\\u200E", "&lrtri;": "\\u22BF", "&lsaquo;": "\\u2039", "&lscr;": "\\uD835\\uDCC1", "&Lscr;": "\\u2112", "&lsh;": "\\u21B0", "&Lsh;": "\\u21B0", "&lsim;": "\\u2272", "&lsime;": "\\u2A8D", "&lsimg;": "\\u2A8F", "&lsqb;": "\\u005B", "&lsquo;": "\\u2018", "&lsquor;": "\\u201A", "&Lstrok;": "\\u0141", "&lstrok;": "\\u0142", "&ltcc;": "\\u2AA6", "&ltcir;": "\\u2A79", "&lt;": "\\u003C", "&lt": "\\u003C", "&LT;": "\\u003C", "&LT": "\\u003C", "&Lt;": "\\u226A", "&ltdot;": "\\u22D6", "&lthree;": "\\u22CB", "&ltimes;": "\\u22C9", "&ltlarr;": "\\u2976", "&ltquest;": "\\u2A7B", "&ltri;": "\\u25C3", "&ltrie;": "\\u22B4", "&ltrif;": "\\u25C2", "&ltrPar;": "\\u2996", "&lurdshar;": "\\u294A", "&luruhar;": "\\u2966", "&lvertneqq;": "\\u2268\\uFE00", "&lvnE;": "\\u2268\\uFE00", "&macr;": "\\u00AF", "&macr": "\\u00AF", "&male;": "\\u2642", "&malt;": "\\u2720", "&maltese;": "\\u2720", "&Map;": "\\u2905", "&map;": "\\u21A6", "&mapsto;": "\\u21A6", "&mapstodown;": "\\u21A7", "&mapstoleft;": "\\u21A4", "&mapstoup;": "\\u21A5", "&marker;": "\\u25AE", "&mcomma;": "\\u2A29", "&Mcy;": "\\u041C", "&mcy;": "\\u043C", "&mdash;": "\\u2014", "&mDDot;": "\\u223A", "&measuredangle;": "\\u2221", "&MediumSpace;": "\\u205F", "&Mellintrf;": "\\u2133", "&Mfr;": "\\uD835\\uDD10", "&mfr;": "\\uD835\\uDD2A", "&mho;": "\\u2127", "&micro;": "\\u00B5", "&micro": "\\u00B5", "&midast;": "\\u002A", "&midcir;": "\\u2AF0", "&mid;": "\\u2223", "&middot;": "\\u00B7", "&middot": "\\u00B7", "&minusb;": "\\u229F", "&minus;": "\\u2212", "&minusd;": "\\u2238", "&minusdu;": "\\u2A2A", "&MinusPlus;": "\\u2213", "&mlcp;": "\\u2ADB", "&mldr;": "\\u2026", "&mnplus;": "\\u2213", "&models;": "\\u22A7", "&Mopf;": "\\uD835\\uDD44", "&mopf;": "\\uD835\\uDD5E", "&mp;": "\\u2213", "&mscr;": "\\uD835\\uDCC2", "&Mscr;": "\\u2133", "&mstpos;": "\\u223E", "&Mu;": "\\u039C", "&mu;": "\\u03BC", "&multimap;": "\\u22B8", "&mumap;": "\\u22B8", "&nabla;": "\\u2207", "&Nacute;": "\\u0143", "&nacute;": "\\u0144", "&nang;": "\\u2220\\u20D2", "&nap;": "\\u2249", "&napE;": "\\u2A70\\u0338", "&napid;": "\\u224B\\u0338", "&napos;": "\\u0149", "&napprox;": "\\u2249", "&natural;": "\\u266E", "&naturals;": "\\u2115", "&natur;": "\\u266E", "&nbsp;": "\\u00A0", "&nbsp": "\\u00A0", "&nbump;": "\\u224E\\u0338", "&nbumpe;": "\\u224F\\u0338", "&ncap;": "\\u2A43", "&Ncaron;": "\\u0147", "&ncaron;": "\\u0148", "&Ncedil;": "\\u0145", "&ncedil;": "\\u0146", "&ncong;": "\\u2247", "&ncongdot;": "\\u2A6D\\u0338", "&ncup;": "\\u2A42", "&Ncy;": "\\u041D", "&ncy;": "\\u043D", "&ndash;": "\\u2013", "&nearhk;": "\\u2924", "&nearr;": "\\u2197", "&neArr;": "\\u21D7", "&nearrow;": "\\u2197", "&ne;": "\\u2260", "&nedot;": "\\u2250\\u0338", "&NegativeMediumSpace;": "\\u200B", "&NegativeThickSpace;": "\\u200B", "&NegativeThinSpace;": "\\u200B", "&NegativeVeryThinSpace;": "\\u200B", "&nequiv;": "\\u2262", "&nesear;": "\\u2928", "&nesim;": "\\u2242\\u0338", "&NestedGreaterGreater;": "\\u226B", "&NestedLessLess;": "\\u226A", "&NewLine;": "\\u000A", "&nexist;": "\\u2204", "&nexists;": "\\u2204", "&Nfr;": "\\uD835\\uDD11", "&nfr;": "\\uD835\\uDD2B", "&ngE;": "\\u2267\\u0338", "&nge;": "\\u2271", "&ngeq;": "\\u2271", "&ngeqq;": "\\u2267\\u0338", "&ngeqslant;": "\\u2A7E\\u0338", "&nges;": "\\u2A7E\\u0338", "&nGg;": "\\u22D9\\u0338", "&ngsim;": "\\u2275", "&nGt;": "\\u226B\\u20D2", "&ngt;": "\\u226F", "&ngtr;": "\\u226F", "&nGtv;": "\\u226B\\u0338", "&nharr;": "\\u21AE", "&nhArr;": "\\u21CE", "&nhpar;": "\\u2AF2", "&ni;": "\\u220B", "&nis;": "\\u22FC", "&nisd;": "\\u22FA", "&niv;": "\\u220B", "&NJcy;": "\\u040A", "&njcy;": "\\u045A", "&nlarr;": "\\u219A", "&nlArr;": "\\u21CD", "&nldr;": "\\u2025", "&nlE;": "\\u2266\\u0338", "&nle;": "\\u2270", "&nleftarrow;": "\\u219A", "&nLeftarrow;": "\\u21CD", "&nleftrightarrow;": "\\u21AE", "&nLeftrightarrow;": "\\u21CE", "&nleq;": "\\u2270", "&nleqq;": "\\u2266\\u0338", "&nleqslant;": "\\u2A7D\\u0338", "&nles;": "\\u2A7D\\u0338", "&nless;": "\\u226E", "&nLl;": "\\u22D8\\u0338", "&nlsim;": "\\u2274", "&nLt;": "\\u226A\\u20D2", "&nlt;": "\\u226E", "&nltri;": "\\u22EA", "&nltrie;": "\\u22EC", "&nLtv;": "\\u226A\\u0338", "&nmid;": "\\u2224", "&NoBreak;": "\\u2060", "&NonBreakingSpace;": "\\u00A0", "&nopf;": "\\uD835\\uDD5F", "&Nopf;": "\\u2115", "&Not;": "\\u2AEC", "&not;": "\\u00AC", "&not": "\\u00AC", "&NotCongruent;": "\\u2262", "&NotCupCap;": "\\u226D", "&NotDoubleVerticalBar;": "\\u2226", "&NotElement;": "\\u2209", "&NotEqual;": "\\u2260", "&NotEqualTilde;": "\\u2242\\u0338", "&NotExists;": "\\u2204", "&NotGreater;": "\\u226F", "&NotGreaterEqual;": "\\u2271", "&NotGreaterFullEqual;": "\\u2267\\u0338", "&NotGreaterGreater;": "\\u226B\\u0338", "&NotGreaterLess;": "\\u2279", "&NotGreaterSlantEqual;": "\\u2A7E\\u0338", "&NotGreaterTilde;": "\\u2275", "&NotHumpDownHump;": "\\u224E\\u0338", "&NotHumpEqual;": "\\u224F\\u0338", "&notin;": "\\u2209", "&notindot;": "\\u22F5\\u0338", "&notinE;": "\\u22F9\\u0338", "&notinva;": "\\u2209", "&notinvb;": "\\u22F7", "&notinvc;": "\\u22F6", "&NotLeftTriangleBar;": "\\u29CF\\u0338", "&NotLeftTriangle;": "\\u22EA", "&NotLeftTriangleEqual;": "\\u22EC", "&NotLess;": "\\u226E", "&NotLessEqual;": "\\u2270", "&NotLessGreater;": "\\u2278", "&NotLessLess;": "\\u226A\\u0338", "&NotLessSlantEqual;": "\\u2A7D\\u0338", "&NotLessTilde;": "\\u2274", "&NotNestedGreaterGreater;": "\\u2AA2\\u0338", "&NotNestedLessLess;": "\\u2AA1\\u0338", "&notni;": "\\u220C", "&notniva;": "\\u220C", "&notnivb;": "\\u22FE", "&notnivc;": "\\u22FD", "&NotPrecedes;": "\\u2280", "&NotPrecedesEqual;": "\\u2AAF\\u0338", "&NotPrecedesSlantEqual;": "\\u22E0", "&NotReverseElement;": "\\u220C", "&NotRightTriangleBar;": "\\u29D0\\u0338", "&NotRightTriangle;": "\\u22EB", "&NotRightTriangleEqual;": "\\u22ED", "&NotSquareSubset;": "\\u228F\\u0338", "&NotSquareSubsetEqual;": "\\u22E2", "&NotSquareSuperset;": "\\u2290\\u0338", "&NotSquareSupersetEqual;": "\\u22E3", "&NotSubset;": "\\u2282\\u20D2", "&NotSubsetEqual;": "\\u2288", "&NotSucceeds;": "\\u2281", "&NotSucceedsEqual;": "\\u2AB0\\u0338", "&NotSucceedsSlantEqual;": "\\u22E1", "&NotSucceedsTilde;": "\\u227F\\u0338", "&NotSuperset;": "\\u2283\\u20D2", "&NotSupersetEqual;": "\\u2289", "&NotTilde;": "\\u2241", "&NotTildeEqual;": "\\u2244", "&NotTildeFullEqual;": "\\u2247", "&NotTildeTilde;": "\\u2249", "&NotVerticalBar;": "\\u2224", "&nparallel;": "\\u2226", "&npar;": "\\u2226", "&nparsl;": "\\u2AFD\\u20E5", "&npart;": "\\u2202\\u0338", "&npolint;": "\\u2A14", "&npr;": "\\u2280", "&nprcue;": "\\u22E0", "&nprec;": "\\u2280", "&npreceq;": "\\u2AAF\\u0338", "&npre;": "\\u2AAF\\u0338", "&nrarrc;": "\\u2933\\u0338", "&nrarr;": "\\u219B", "&nrArr;": "\\u21CF", "&nrarrw;": "\\u219D\\u0338", "&nrightarrow;": "\\u219B", "&nRightarrow;": "\\u21CF", "&nrtri;": "\\u22EB", "&nrtrie;": "\\u22ED", "&nsc;": "\\u2281", "&nsccue;": "\\u22E1", "&nsce;": "\\u2AB0\\u0338", "&Nscr;": "\\uD835\\uDCA9", "&nscr;": "\\uD835\\uDCC3", "&nshortmid;": "\\u2224", "&nshortparallel;": "\\u2226", "&nsim;": "\\u2241", "&nsime;": "\\u2244", "&nsimeq;": "\\u2244", "&nsmid;": "\\u2224", "&nspar;": "\\u2226", "&nsqsube;": "\\u22E2", "&nsqsupe;": "\\u22E3", "&nsub;": "\\u2284", "&nsubE;": "\\u2AC5\\u0338", "&nsube;": "\\u2288", "&nsubset;": "\\u2282\\u20D2", "&nsubseteq;": "\\u2288", "&nsubseteqq;": "\\u2AC5\\u0338", "&nsucc;": "\\u2281", "&nsucceq;": "\\u2AB0\\u0338", "&nsup;": "\\u2285", "&nsupE;": "\\u2AC6\\u0338", "&nsupe;": "\\u2289", "&nsupset;": "\\u2283\\u20D2", "&nsupseteq;": "\\u2289", "&nsupseteqq;": "\\u2AC6\\u0338", "&ntgl;": "\\u2279", "&Ntilde;": "\\u00D1", "&Ntilde": "\\u00D1", "&ntilde;": "\\u00F1", "&ntilde": "\\u00F1", "&ntlg;": "\\u2278", "&ntriangleleft;": "\\u22EA", "&ntrianglelefteq;": "\\u22EC", "&ntriangleright;": "\\u22EB", "&ntrianglerighteq;": "\\u22ED", "&Nu;": "\\u039D", "&nu;": "\\u03BD", "&num;": "\\u0023", "&numero;": "\\u2116", "&numsp;": "\\u2007", "&nvap;": "\\u224D\\u20D2", "&nvdash;": "\\u22AC", "&nvDash;": "\\u22AD", "&nVdash;": "\\u22AE", "&nVDash;": "\\u22AF", "&nvge;": "\\u2265\\u20D2", "&nvgt;": "\\u003E\\u20D2", "&nvHarr;": "\\u2904", "&nvinfin;": "\\u29DE", "&nvlArr;": "\\u2902", "&nvle;": "\\u2264\\u20D2", "&nvlt;": "\\u003C\\u20D2", "&nvltrie;": "\\u22B4\\u20D2", "&nvrArr;": "\\u2903", "&nvrtrie;": "\\u22B5\\u20D2", "&nvsim;": "\\u223C\\u20D2", "&nwarhk;": "\\u2923", "&nwarr;": "\\u2196", "&nwArr;": "\\u21D6", "&nwarrow;": "\\u2196", "&nwnear;": "\\u2927", "&Oacute;": "\\u00D3", "&Oacute": "\\u00D3", "&oacute;": "\\u00F3", "&oacute": "\\u00F3", "&oast;": "\\u229B", "&Ocirc;": "\\u00D4", "&Ocirc": "\\u00D4", "&ocirc;": "\\u00F4", "&ocirc": "\\u00F4", "&ocir;": "\\u229A", "&Ocy;": "\\u041E", "&ocy;": "\\u043E", "&odash;": "\\u229D", "&Odblac;": "\\u0150", "&odblac;": "\\u0151", "&odiv;": "\\u2A38", "&odot;": "\\u2299", "&odsold;": "\\u29BC", "&OElig;": "\\u0152", "&oelig;": "\\u0153", "&ofcir;": "\\u29BF", "&Ofr;": "\\uD835\\uDD12", "&ofr;": "\\uD835\\uDD2C", "&ogon;": "\\u02DB", "&Ograve;": "\\u00D2", "&Ograve": "\\u00D2", "&ograve;": "\\u00F2", "&ograve": "\\u00F2", "&ogt;": "\\u29C1", "&ohbar;": "\\u29B5", "&ohm;": "\\u03A9", "&oint;": "\\u222E", "&olarr;": "\\u21BA", "&olcir;": "\\u29BE", "&olcross;": "\\u29BB", "&oline;": "\\u203E", "&olt;": "\\u29C0", "&Omacr;": "\\u014C", "&omacr;": "\\u014D", "&Omega;": "\\u03A9", "&omega;": "\\u03C9", "&Omicron;": "\\u039F", "&omicron;": "\\u03BF", "&omid;": "\\u29B6", "&ominus;": "\\u2296", "&Oopf;": "\\uD835\\uDD46", "&oopf;": "\\uD835\\uDD60", "&opar;": "\\u29B7", "&OpenCurlyDoubleQuote;": "\\u201C", "&OpenCurlyQuote;": "\\u2018", "&operp;": "\\u29B9", "&oplus;": "\\u2295", "&orarr;": "\\u21BB", "&Or;": "\\u2A54", "&or;": "\\u2228", "&ord;": "\\u2A5D", "&order;": "\\u2134", "&orderof;": "\\u2134", "&ordf;": "\\u00AA", "&ordf": "\\u00AA", "&ordm;": "\\u00BA", "&ordm": "\\u00BA", "&origof;": "\\u22B6", "&oror;": "\\u2A56", "&orslope;": "\\u2A57", "&orv;": "\\u2A5B", "&oS;": "\\u24C8", "&Oscr;": "\\uD835\\uDCAA", "&oscr;": "\\u2134", "&Oslash;": "\\u00D8", "&Oslash": "\\u00D8", "&oslash;": "\\u00F8", "&oslash": "\\u00F8", "&osol;": "\\u2298", "&Otilde;": "\\u00D5", "&Otilde": "\\u00D5", "&otilde;": "\\u00F5", "&otilde": "\\u00F5", "&otimesas;": "\\u2A36", "&Otimes;": "\\u2A37", "&otimes;": "\\u2297", "&Ouml;": "\\u00D6", "&Ouml": "\\u00D6", "&ouml;": "\\u00F6", "&ouml": "\\u00F6", "&ovbar;": "\\u233D", "&OverBar;": "\\u203E", "&OverBrace;": "\\u23DE", "&OverBracket;": "\\u23B4", "&OverParenthesis;": "\\u23DC", "&para;": "\\u00B6", "&para": "\\u00B6", "&parallel;": "\\u2225", "&par;": "\\u2225", "&parsim;": "\\u2AF3", "&parsl;": "\\u2AFD", "&part;": "\\u2202", "&PartialD;": "\\u2202", "&Pcy;": "\\u041F", "&pcy;": "\\u043F", "&percnt;": "\\u0025", "&period;": "\\u002E", "&permil;": "\\u2030", "&perp;": "\\u22A5", "&pertenk;": "\\u2031", "&Pfr;": "\\uD835\\uDD13", "&pfr;": "\\uD835\\uDD2D", "&Phi;": "\\u03A6", "&phi;": "\\u03C6", "&phiv;": "\\u03D5", "&phmmat;": "\\u2133", "&phone;": "\\u260E", "&Pi;": "\\u03A0", "&pi;": "\\u03C0", "&pitchfork;": "\\u22D4", "&piv;": "\\u03D6", "&planck;": "\\u210F", "&planckh;": "\\u210E", "&plankv;": "\\u210F", "&plusacir;": "\\u2A23", "&plusb;": "\\u229E", "&pluscir;": "\\u2A22", "&plus;": "\\u002B", "&plusdo;": "\\u2214", "&plusdu;": "\\u2A25", "&pluse;": "\\u2A72", "&PlusMinus;": "\\u00B1", "&plusmn;": "\\u00B1", "&plusmn": "\\u00B1", "&plussim;": "\\u2A26", "&plustwo;": "\\u2A27", "&pm;": "\\u00B1", "&Poincareplane;": "\\u210C", "&pointint;": "\\u2A15", "&popf;": "\\uD835\\uDD61", "&Popf;": "\\u2119", "&pound;": "\\u00A3", "&pound": "\\u00A3", "&prap;": "\\u2AB7", "&Pr;": "\\u2ABB", "&pr;": "\\u227A", "&prcue;": "\\u227C", "&precapprox;": "\\u2AB7", "&prec;": "\\u227A", "&preccurlyeq;": "\\u227C", "&Precedes;": "\\u227A", "&PrecedesEqual;": "\\u2AAF", "&PrecedesSlantEqual;": "\\u227C", "&PrecedesTilde;": "\\u227E", "&preceq;": "\\u2AAF", "&precnapprox;": "\\u2AB9", "&precneqq;": "\\u2AB5", "&precnsim;": "\\u22E8", "&pre;": "\\u2AAF", "&prE;": "\\u2AB3", "&precsim;": "\\u227E", "&prime;": "\\u2032", "&Prime;": "\\u2033", "&primes;": "\\u2119", "&prnap;": "\\u2AB9", "&prnE;": "\\u2AB5", "&prnsim;": "\\u22E8", "&prod;": "\\u220F", "&Product;": "\\u220F", "&profalar;": "\\u232E", "&profline;": "\\u2312", "&profsurf;": "\\u2313", "&prop;": "\\u221D", "&Proportional;": "\\u221D", "&Proportion;": "\\u2237", "&propto;": "\\u221D", "&prsim;": "\\u227E", "&prurel;": "\\u22B0", "&Pscr;": "\\uD835\\uDCAB", "&pscr;": "\\uD835\\uDCC5", "&Psi;": "\\u03A8", "&psi;": "\\u03C8", "&puncsp;": "\\u2008", "&Qfr;": "\\uD835\\uDD14", "&qfr;": "\\uD835\\uDD2E", "&qint;": "\\u2A0C", "&qopf;": "\\uD835\\uDD62", "&Qopf;": "\\u211A", "&qprime;": "\\u2057", "&Qscr;": "\\uD835\\uDCAC", "&qscr;": "\\uD835\\uDCC6", "&quaternions;": "\\u210D", "&quatint;": "\\u2A16", "&quest;": "\\u003F", "&questeq;": "\\u225F", "&quot;": "\\u0022", "&quot": "\\u0022", "&QUOT;": "\\u0022", "&QUOT": "\\u0022", "&rAarr;": "\\u21DB", "&race;": "\\u223D\\u0331", "&Racute;": "\\u0154", "&racute;": "\\u0155", "&radic;": "\\u221A", "&raemptyv;": "\\u29B3", "&rang;": "\\u27E9", "&Rang;": "\\u27EB", "&rangd;": "\\u2992", "&range;": "\\u29A5", "&rangle;": "\\u27E9", "&raquo;": "\\u00BB", "&raquo": "\\u00BB", "&rarrap;": "\\u2975", "&rarrb;": "\\u21E5", "&rarrbfs;": "\\u2920", "&rarrc;": "\\u2933", "&rarr;": "\\u2192", "&Rarr;": "\\u21A0", "&rArr;": "\\u21D2", "&rarrfs;": "\\u291E", "&rarrhk;": "\\u21AA", "&rarrlp;": "\\u21AC", "&rarrpl;": "\\u2945", "&rarrsim;": "\\u2974", "&Rarrtl;": "\\u2916", "&rarrtl;": "\\u21A3", "&rarrw;": "\\u219D", "&ratail;": "\\u291A", "&rAtail;": "\\u291C", "&ratio;": "\\u2236", "&rationals;": "\\u211A", "&rbarr;": "\\u290D", "&rBarr;": "\\u290F", "&RBarr;": "\\u2910", "&rbbrk;": "\\u2773", "&rbrace;": "\\u007D", "&rbrack;": "\\u005D", "&rbrke;": "\\u298C", "&rbrksld;": "\\u298E", "&rbrkslu;": "\\u2990", "&Rcaron;": "\\u0158", "&rcaron;": "\\u0159", "&Rcedil;": "\\u0156", "&rcedil;": "\\u0157", "&rceil;": "\\u2309", "&rcub;": "\\u007D", "&Rcy;": "\\u0420", "&rcy;": "\\u0440", "&rdca;": "\\u2937", "&rdldhar;": "\\u2969", "&rdquo;": "\\u201D", "&rdquor;": "\\u201D", "&rdsh;": "\\u21B3", "&real;": "\\u211C", "&realine;": "\\u211B", "&realpart;": "\\u211C", "&reals;": "\\u211D", "&Re;": "\\u211C", "&rect;": "\\u25AD", "&reg;": "\\u00AE", "&reg": "\\u00AE", "&REG;": "\\u00AE", "&REG": "\\u00AE", "&ReverseElement;": "\\u220B", "&ReverseEquilibrium;": "\\u21CB", "&ReverseUpEquilibrium;": "\\u296F", "&rfisht;": "\\u297D", "&rfloor;": "\\u230B", "&rfr;": "\\uD835\\uDD2F", "&Rfr;": "\\u211C", "&rHar;": "\\u2964", "&rhard;": "\\u21C1", "&rharu;": "\\u21C0", "&rharul;": "\\u296C", "&Rho;": "\\u03A1", "&rho;": "\\u03C1", "&rhov;": "\\u03F1", "&RightAngleBracket;": "\\u27E9", "&RightArrowBar;": "\\u21E5", "&rightarrow;": "\\u2192", "&RightArrow;": "\\u2192", "&Rightarrow;": "\\u21D2", "&RightArrowLeftArrow;": "\\u21C4", "&rightarrowtail;": "\\u21A3", "&RightCeiling;": "\\u2309", "&RightDoubleBracket;": "\\u27E7", "&RightDownTeeVector;": "\\u295D", "&RightDownVectorBar;": "\\u2955", "&RightDownVector;": "\\u21C2", "&RightFloor;": "\\u230B", "&rightharpoondown;": "\\u21C1", "&rightharpoonup;": "\\u21C0", "&rightleftarrows;": "\\u21C4", "&rightleftharpoons;": "\\u21CC", "&rightrightarrows;": "\\u21C9", "&rightsquigarrow;": "\\u219D", "&RightTeeArrow;": "\\u21A6", "&RightTee;": "\\u22A2", "&RightTeeVector;": "\\u295B", "&rightthreetimes;": "\\u22CC", "&RightTriangleBar;": "\\u29D0", "&RightTriangle;": "\\u22B3", "&RightTriangleEqual;": "\\u22B5", "&RightUpDownVector;": "\\u294F", "&RightUpTeeVector;": "\\u295C", "&RightUpVectorBar;": "\\u2954", "&RightUpVector;": "\\u21BE", "&RightVectorBar;": "\\u2953", "&RightVector;": "\\u21C0", "&ring;": "\\u02DA", "&risingdotseq;": "\\u2253", "&rlarr;": "\\u21C4", "&rlhar;": "\\u21CC", "&rlm;": "\\u200F", "&rmoustache;": "\\u23B1", "&rmoust;": "\\u23B1", "&rnmid;": "\\u2AEE", "&roang;": "\\u27ED", "&roarr;": "\\u21FE", "&robrk;": "\\u27E7", "&ropar;": "\\u2986", "&ropf;": "\\uD835\\uDD63", "&Ropf;": "\\u211D", "&roplus;": "\\u2A2E", "&rotimes;": "\\u2A35", "&RoundImplies;": "\\u2970", "&rpar;": "\\u0029", "&rpargt;": "\\u2994", "&rppolint;": "\\u2A12", "&rrarr;": "\\u21C9", "&Rrightarrow;": "\\u21DB", "&rsaquo;": "\\u203A", "&rscr;": "\\uD835\\uDCC7", "&Rscr;": "\\u211B", "&rsh;": "\\u21B1", "&Rsh;": "\\u21B1", "&rsqb;": "\\u005D", "&rsquo;": "\\u2019", "&rsquor;": "\\u2019", "&rthree;": "\\u22CC", "&rtimes;": "\\u22CA", "&rtri;": "\\u25B9", "&rtrie;": "\\u22B5", "&rtrif;": "\\u25B8", "&rtriltri;": "\\u29CE", "&RuleDelayed;": "\\u29F4", "&ruluhar;": "\\u2968", "&rx;": "\\u211E", "&Sacute;": "\\u015A", "&sacute;": "\\u015B", "&sbquo;": "\\u201A", "&scap;": "\\u2AB8", "&Scaron;": "\\u0160", "&scaron;": "\\u0161", "&Sc;": "\\u2ABC", "&sc;": "\\u227B", "&sccue;": "\\u227D", "&sce;": "\\u2AB0", "&scE;": "\\u2AB4", "&Scedil;": "\\u015E", "&scedil;": "\\u015F", "&Scirc;": "\\u015C", "&scirc;": "\\u015D", "&scnap;": "\\u2ABA", "&scnE;": "\\u2AB6", "&scnsim;": "\\u22E9", "&scpolint;": "\\u2A13", "&scsim;": "\\u227F", "&Scy;": "\\u0421", "&scy;": "\\u0441", "&sdotb;": "\\u22A1", "&sdot;": "\\u22C5", "&sdote;": "\\u2A66", "&searhk;": "\\u2925", "&searr;": "\\u2198", "&seArr;": "\\u21D8", "&searrow;": "\\u2198", "&sect;": "\\u00A7", "&sect": "\\u00A7", "&semi;": "\\u003B", "&seswar;": "\\u2929", "&setminus;": "\\u2216", "&setmn;": "\\u2216", "&sext;": "\\u2736", "&Sfr;": "\\uD835\\uDD16", "&sfr;": "\\uD835\\uDD30", "&sfrown;": "\\u2322", "&sharp;": "\\u266F", "&SHCHcy;": "\\u0429", "&shchcy;": "\\u0449", "&SHcy;": "\\u0428", "&shcy;": "\\u0448", "&ShortDownArrow;": "\\u2193", "&ShortLeftArrow;": "\\u2190", "&shortmid;": "\\u2223", "&shortparallel;": "\\u2225", "&ShortRightArrow;": "\\u2192", "&ShortUpArrow;": "\\u2191", "&shy;": "\\u00AD", "&shy": "\\u00AD", "&Sigma;": "\\u03A3", "&sigma;": "\\u03C3", "&sigmaf;": "\\u03C2", "&sigmav;": "\\u03C2", "&sim;": "\\u223C", "&simdot;": "\\u2A6A", "&sime;": "\\u2243", "&simeq;": "\\u2243", "&simg;": "\\u2A9E", "&simgE;": "\\u2AA0", "&siml;": "\\u2A9D", "&simlE;": "\\u2A9F", "&simne;": "\\u2246", "&simplus;": "\\u2A24", "&simrarr;": "\\u2972", "&slarr;": "\\u2190", "&SmallCircle;": "\\u2218", "&smallsetminus;": "\\u2216", "&smashp;": "\\u2A33", "&smeparsl;": "\\u29E4", "&smid;": "\\u2223", "&smile;": "\\u2323", "&smt;": "\\u2AAA", "&smte;": "\\u2AAC", "&smtes;": "\\u2AAC\\uFE00", "&SOFTcy;": "\\u042C", "&softcy;": "\\u044C", "&solbar;": "\\u233F", "&solb;": "\\u29C4", "&sol;": "\\u002F", "&Sopf;": "\\uD835\\uDD4A", "&sopf;": "\\uD835\\uDD64", "&spades;": "\\u2660", "&spadesuit;": "\\u2660", "&spar;": "\\u2225", "&sqcap;": "\\u2293", "&sqcaps;": "\\u2293\\uFE00", "&sqcup;": "\\u2294", "&sqcups;": "\\u2294\\uFE00", "&Sqrt;": "\\u221A", "&sqsub;": "\\u228F", "&sqsube;": "\\u2291", "&sqsubset;": "\\u228F", "&sqsubseteq;": "\\u2291", "&sqsup;": "\\u2290", "&sqsupe;": "\\u2292", "&sqsupset;": "\\u2290", "&sqsupseteq;": "\\u2292", "&square;": "\\u25A1", "&Square;": "\\u25A1", "&SquareIntersection;": "\\u2293", "&SquareSubset;": "\\u228F", "&SquareSubsetEqual;": "\\u2291", "&SquareSuperset;": "\\u2290", "&SquareSupersetEqual;": "\\u2292", "&SquareUnion;": "\\u2294", "&squarf;": "\\u25AA", "&squ;": "\\u25A1", "&squf;": "\\u25AA", "&srarr;": "\\u2192", "&Sscr;": "\\uD835\\uDCAE", "&sscr;": "\\uD835\\uDCC8", "&ssetmn;": "\\u2216", "&ssmile;": "\\u2323", "&sstarf;": "\\u22C6", "&Star;": "\\u22C6", "&star;": "\\u2606", "&starf;": "\\u2605", "&straightepsilon;": "\\u03F5", "&straightphi;": "\\u03D5", "&strns;": "\\u00AF", "&sub;": "\\u2282", "&Sub;": "\\u22D0", "&subdot;": "\\u2ABD", "&subE;": "\\u2AC5", "&sube;": "\\u2286", "&subedot;": "\\u2AC3", "&submult;": "\\u2AC1", "&subnE;": "\\u2ACB", "&subne;": "\\u228A", "&subplus;": "\\u2ABF", "&subrarr;": "\\u2979", "&subset;": "\\u2282", "&Subset;": "\\u22D0", "&subseteq;": "\\u2286", "&subseteqq;": "\\u2AC5", "&SubsetEqual;": "\\u2286", "&subsetneq;": "\\u228A", "&subsetneqq;": "\\u2ACB", "&subsim;": "\\u2AC7", "&subsub;": "\\u2AD5", "&subsup;": "\\u2AD3", "&succapprox;": "\\u2AB8", "&succ;": "\\u227B", "&succcurlyeq;": "\\u227D", "&Succeeds;": "\\u227B", "&SucceedsEqual;": "\\u2AB0", "&SucceedsSlantEqual;": "\\u227D", "&SucceedsTilde;": "\\u227F", "&succeq;": "\\u2AB0", "&succnapprox;": "\\u2ABA", "&succneqq;": "\\u2AB6", "&succnsim;": "\\u22E9", "&succsim;": "\\u227F", "&SuchThat;": "\\u220B", "&sum;": "\\u2211", "&Sum;": "\\u2211", "&sung;": "\\u266A", "&sup1;": "\\u00B9", "&sup1": "\\u00B9", "&sup2;": "\\u00B2", "&sup2": "\\u00B2", "&sup3;": "\\u00B3", "&sup3": "\\u00B3", "&sup;": "\\u2283", "&Sup;": "\\u22D1", "&supdot;": "\\u2ABE", "&supdsub;": "\\u2AD8", "&supE;": "\\u2AC6", "&supe;": "\\u2287", "&supedot;": "\\u2AC4", "&Superset;": "\\u2283", "&SupersetEqual;": "\\u2287", "&suphsol;": "\\u27C9", "&suphsub;": "\\u2AD7", "&suplarr;": "\\u297B", "&supmult;": "\\u2AC2", "&supnE;": "\\u2ACC", "&supne;": "\\u228B", "&supplus;": "\\u2AC0", "&supset;": "\\u2283", "&Supset;": "\\u22D1", "&supseteq;": "\\u2287", "&supseteqq;": "\\u2AC6", "&supsetneq;": "\\u228B", "&supsetneqq;": "\\u2ACC", "&supsim;": "\\u2AC8", "&supsub;": "\\u2AD4", "&supsup;": "\\u2AD6", "&swarhk;": "\\u2926", "&swarr;": "\\u2199", "&swArr;": "\\u21D9", "&swarrow;": "\\u2199", "&swnwar;": "\\u292A", "&szlig;": "\\u00DF", "&szlig": "\\u00DF", "&Tab;": "\\u0009", "&target;": "\\u2316", "&Tau;": "\\u03A4", "&tau;": "\\u03C4", "&tbrk;": "\\u23B4", "&Tcaron;": "\\u0164", "&tcaron;": "\\u0165", "&Tcedil;": "\\u0162", "&tcedil;": "\\u0163", "&Tcy;": "\\u0422", "&tcy;": "\\u0442", "&tdot;": "\\u20DB", "&telrec;": "\\u2315", "&Tfr;": "\\uD835\\uDD17", "&tfr;": "\\uD835\\uDD31", "&there4;": "\\u2234", "&therefore;": "\\u2234", "&Therefore;": "\\u2234", "&Theta;": "\\u0398", "&theta;": "\\u03B8", "&thetasym;": "\\u03D1", "&thetav;": "\\u03D1", "&thickapprox;": "\\u2248", "&thicksim;": "\\u223C", "&ThickSpace;": "\\u205F\\u200A", "&ThinSpace;": "\\u2009", "&thinsp;": "\\u2009", "&thkap;": "\\u2248", "&thksim;": "\\u223C", "&THORN;": "\\u00DE", "&THORN": "\\u00DE", "&thorn;": "\\u00FE", "&thorn": "\\u00FE", "&tilde;": "\\u02DC", "&Tilde;": "\\u223C", "&TildeEqual;": "\\u2243", "&TildeFullEqual;": "\\u2245", "&TildeTilde;": "\\u2248", "&timesbar;": "\\u2A31", "&timesb;": "\\u22A0", "&times;": "\\u00D7", "&times": "\\u00D7", "&timesd;": "\\u2A30", "&tint;": "\\u222D", "&toea;": "\\u2928", "&topbot;": "\\u2336", "&topcir;": "\\u2AF1", "&top;": "\\u22A4", "&Topf;": "\\uD835\\uDD4B", "&topf;": "\\uD835\\uDD65", "&topfork;": "\\u2ADA", "&tosa;": "\\u2929", "&tprime;": "\\u2034", "&trade;": "\\u2122", "&TRADE;": "\\u2122", "&triangle;": "\\u25B5", "&triangledown;": "\\u25BF", "&triangleleft;": "\\u25C3", "&trianglelefteq;": "\\u22B4", "&triangleq;": "\\u225C", "&triangleright;": "\\u25B9", "&trianglerighteq;": "\\u22B5", "&tridot;": "\\u25EC", "&trie;": "\\u225C", "&triminus;": "\\u2A3A", "&TripleDot;": "\\u20DB", "&triplus;": "\\u2A39", "&trisb;": "\\u29CD", "&tritime;": "\\u2A3B", "&trpezium;": "\\u23E2", "&Tscr;": "\\uD835\\uDCAF", "&tscr;": "\\uD835\\uDCC9", "&TScy;": "\\u0426", "&tscy;": "\\u0446", "&TSHcy;": "\\u040B", "&tshcy;": "\\u045B", "&Tstrok;": "\\u0166", "&tstrok;": "\\u0167", "&twixt;": "\\u226C", "&twoheadleftarrow;": "\\u219E", "&twoheadrightarrow;": "\\u21A0", "&Uacute;": "\\u00DA", "&Uacute": "\\u00DA", "&uacute;": "\\u00FA", "&uacute": "\\u00FA", "&uarr;": "\\u2191", "&Uarr;": "\\u219F", "&uArr;": "\\u21D1", "&Uarrocir;": "\\u2949", "&Ubrcy;": "\\u040E", "&ubrcy;": "\\u045E", "&Ubreve;": "\\u016C", "&ubreve;": "\\u016D", "&Ucirc;": "\\u00DB", "&Ucirc": "\\u00DB", "&ucirc;": "\\u00FB", "&ucirc": "\\u00FB", "&Ucy;": "\\u0423", "&ucy;": "\\u0443", "&udarr;": "\\u21C5", "&Udblac;": "\\u0170", "&udblac;": "\\u0171", "&udhar;": "\\u296E", "&ufisht;": "\\u297E", "&Ufr;": "\\uD835\\uDD18", "&ufr;": "\\uD835\\uDD32", "&Ugrave;": "\\u00D9", "&Ugrave": "\\u00D9", "&ugrave;": "\\u00F9", "&ugrave": "\\u00F9", "&uHar;": "\\u2963", "&uharl;": "\\u21BF", "&uharr;": "\\u21BE", "&uhblk;": "\\u2580", "&ulcorn;": "\\u231C", "&ulcorner;": "\\u231C", "&ulcrop;": "\\u230F", "&ultri;": "\\u25F8", "&Umacr;": "\\u016A", "&umacr;": "\\u016B", "&uml;": "\\u00A8", "&uml": "\\u00A8", "&UnderBar;": "\\u005F", "&UnderBrace;": "\\u23DF", "&UnderBracket;": "\\u23B5", "&UnderParenthesis;": "\\u23DD", "&Union;": "\\u22C3", "&UnionPlus;": "\\u228E", "&Uogon;": "\\u0172", "&uogon;": "\\u0173", "&Uopf;": "\\uD835\\uDD4C", "&uopf;": "\\uD835\\uDD66", "&UpArrowBar;": "\\u2912", "&uparrow;": "\\u2191", "&UpArrow;": "\\u2191", "&Uparrow;": "\\u21D1", "&UpArrowDownArrow;": "\\u21C5", "&updownarrow;": "\\u2195", "&UpDownArrow;": "\\u2195", "&Updownarrow;": "\\u21D5", "&UpEquilibrium;": "\\u296E", "&upharpoonleft;": "\\u21BF", "&upharpoonright;": "\\u21BE", "&uplus;": "\\u228E", "&UpperLeftArrow;": "\\u2196", "&UpperRightArrow;": "\\u2197", "&upsi;": "\\u03C5", "&Upsi;": "\\u03D2", "&upsih;": "\\u03D2", "&Upsilon;": "\\u03A5", "&upsilon;": "\\u03C5", "&UpTeeArrow;": "\\u21A5", "&UpTee;": "\\u22A5", "&upuparrows;": "\\u21C8", "&urcorn;": "\\u231D", "&urcorner;": "\\u231D", "&urcrop;": "\\u230E", "&Uring;": "\\u016E", "&uring;": "\\u016F", "&urtri;": "\\u25F9", "&Uscr;": "\\uD835\\uDCB0", "&uscr;": "\\uD835\\uDCCA", "&utdot;": "\\u22F0", "&Utilde;": "\\u0168", "&utilde;": "\\u0169", "&utri;": "\\u25B5", "&utrif;": "\\u25B4", "&uuarr;": "\\u21C8", "&Uuml;": "\\u00DC", "&Uuml": "\\u00DC", "&uuml;": "\\u00FC", "&uuml": "\\u00FC", "&uwangle;": "\\u29A7", "&vangrt;": "\\u299C", "&varepsilon;": "\\u03F5", "&varkappa;": "\\u03F0", "&varnothing;": "\\u2205", "&varphi;": "\\u03D5", "&varpi;": "\\u03D6", "&varpropto;": "\\u221D", "&varr;": "\\u2195", "&vArr;": "\\u21D5", "&varrho;": "\\u03F1", "&varsigma;": "\\u03C2", "&varsubsetneq;": "\\u228A\\uFE00", "&varsubsetneqq;": "\\u2ACB\\uFE00", "&varsupsetneq;": "\\u228B\\uFE00", "&varsupsetneqq;": "\\u2ACC\\uFE00", "&vartheta;": "\\u03D1", "&vartriangleleft;": "\\u22B2", "&vartriangleright;": "\\u22B3", "&vBar;": "\\u2AE8", "&Vbar;": "\\u2AEB", "&vBarv;": "\\u2AE9", "&Vcy;": "\\u0412", "&vcy;": "\\u0432", "&vdash;": "\\u22A2", "&vDash;": "\\u22A8", "&Vdash;": "\\u22A9", "&VDash;": "\\u22AB", "&Vdashl;": "\\u2AE6", "&veebar;": "\\u22BB", "&vee;": "\\u2228", "&Vee;": "\\u22C1", "&veeeq;": "\\u225A", "&vellip;": "\\u22EE", "&verbar;": "\\u007C", "&Verbar;": "\\u2016", "&vert;": "\\u007C", "&Vert;": "\\u2016", "&VerticalBar;": "\\u2223", "&VerticalLine;": "\\u007C", "&VerticalSeparator;": "\\u2758", "&VerticalTilde;": "\\u2240", "&VeryThinSpace;": "\\u200A", "&Vfr;": "\\uD835\\uDD19", "&vfr;": "\\uD835\\uDD33", "&vltri;": "\\u22B2", "&vnsub;": "\\u2282\\u20D2", "&vnsup;": "\\u2283\\u20D2", "&Vopf;": "\\uD835\\uDD4D", "&vopf;": "\\uD835\\uDD67", "&vprop;": "\\u221D", "&vrtri;": "\\u22B3", "&Vscr;": "\\uD835\\uDCB1", "&vscr;": "\\uD835\\uDCCB", "&vsubnE;": "\\u2ACB\\uFE00", "&vsubne;": "\\u228A\\uFE00", "&vsupnE;": "\\u2ACC\\uFE00", "&vsupne;": "\\u228B\\uFE00", "&Vvdash;": "\\u22AA", "&vzigzag;": "\\u299A", "&Wcirc;": "\\u0174", "&wcirc;": "\\u0175", "&wedbar;": "\\u2A5F", "&wedge;": "\\u2227", "&Wedge;": "\\u22C0", "&wedgeq;": "\\u2259", "&weierp;": "\\u2118", "&Wfr;": "\\uD835\\uDD1A", "&wfr;": "\\uD835\\uDD34", "&Wopf;": "\\uD835\\uDD4E", "&wopf;": "\\uD835\\uDD68", "&wp;": "\\u2118", "&wr;": "\\u2240", "&wreath;": "\\u2240", "&Wscr;": "\\uD835\\uDCB2", "&wscr;": "\\uD835\\uDCCC", "&xcap;": "\\u22C2", "&xcirc;": "\\u25EF", "&xcup;": "\\u22C3", "&xdtri;": "\\u25BD", "&Xfr;": "\\uD835\\uDD1B", "&xfr;": "\\uD835\\uDD35", "&xharr;": "\\u27F7", "&xhArr;": "\\u27FA", "&Xi;": "\\u039E", "&xi;": "\\u03BE", "&xlarr;": "\\u27F5", "&xlArr;": "\\u27F8", "&xmap;": "\\u27FC", "&xnis;": "\\u22FB", "&xodot;": "\\u2A00", "&Xopf;": "\\uD835\\uDD4F", "&xopf;": "\\uD835\\uDD69", "&xoplus;": "\\u2A01", "&xotime;": "\\u2A02", "&xrarr;": "\\u27F6", "&xrArr;": "\\u27F9", "&Xscr;": "\\uD835\\uDCB3", "&xscr;": "\\uD835\\uDCCD", "&xsqcup;": "\\u2A06", "&xuplus;": "\\u2A04", "&xutri;": "\\u25B3", "&xvee;": "\\u22C1", "&xwedge;": "\\u22C0", "&Yacute;": "\\u00DD", "&Yacute": "\\u00DD", "&yacute;": "\\u00FD", "&yacute": "\\u00FD", "&YAcy;": "\\u042F", "&yacy;": "\\u044F", "&Ycirc;": "\\u0176", "&ycirc;": "\\u0177", "&Ycy;": "\\u042B", "&ycy;": "\\u044B", "&yen;": "\\u00A5", "&yen": "\\u00A5", "&Yfr;": "\\uD835\\uDD1C", "&yfr;": "\\uD835\\uDD36", "&YIcy;": "\\u0407", "&yicy;": "\\u0457", "&Yopf;": "\\uD835\\uDD50", "&yopf;": "\\uD835\\uDD6A", "&Yscr;": "\\uD835\\uDCB4", "&yscr;": "\\uD835\\uDCCE", "&YUcy;": "\\u042E", "&yucy;": "\\u044E", "&yuml;": "\\u00FF", "&yuml": "\\u00FF", "&Yuml;": "\\u0178", "&Zacute;": "\\u0179", "&zacute;": "\\u017A", "&Zcaron;": "\\u017D", "&zcaron;": "\\u017E", "&Zcy;": "\\u0417", "&zcy;": "\\u0437", "&Zdot;": "\\u017B", "&zdot;": "\\u017C", "&zeetrf;": "\\u2128", "&ZeroWidthSpace;": "\\u200B", "&Zeta;": "\\u0396", "&zeta;": "\\u03B6", "&zfr;": "\\uD835\\uDD37", "&Zfr;": "\\u2128", "&ZHcy;": "\\u0416", "&zhcy;": "\\u0436", "&zigrarr;": "\\u21DD", "&zopf;": "\\uD835\\uDD6B", "&Zopf;": "\\u2124", "&Zscr;": "\\uD835\\uDCB5", "&zscr;": "\\uD835\\uDCCF", "&zwj;": "\\u200D", "&zwnj;": "\\u200C"}`);
var htmlNumEntities = JSON.parse(`{"193": "\\u00C1", "193": "\\u00C1", "225": "\\u00E1", "225": "\\u00E1", "258": "\\u0102", "259": "\\u0103", "8766": "\\u223E", "8767": "\\u223F", "8766": "\\u223E", "819": "\\u0333", "194": "\\u00C2", "194": "\\u00C2", "226": "\\u00E2", "226": "\\u00E2", "180": "\\u00B4", "180": "\\u00B4", "1040": "\\u0410", "1072": "\\u0430", "198": "\\u00C6", "198": "\\u00C6", "230": "\\u00E6", "230": "\\u00E6", "8289": "\\u2061", "120068": "\\uD835\\uDD04", "120094": "\\uD835\\uDD1E", "192": "\\u00C0", "192": "\\u00C0", "224": "\\u00E0", "224": "\\u00E0", "8501": "\\u2135", "8501": "\\u2135", "913": "\\u0391", "945": "\\u03B1", "256": "\\u0100", "257": "\\u0101", "10815": "\\u2A3F", "38": "\\u0026", "38": "\\u0026", "38": "\\u0026", "38": "\\u0026", "10837": "\\u2A55", "10835": "\\u2A53", "8743": "\\u2227", "10844": "\\u2A5C", "10840": "\\u2A58", "10842": "\\u2A5A", "8736": "\\u2220", "10660": "\\u29A4", "8736": "\\u2220", "10664": "\\u29A8", "10665": "\\u29A9", "10666": "\\u29AA", "10667": "\\u29AB", "10668": "\\u29AC", "10669": "\\u29AD", "10670": "\\u29AE", "10671": "\\u29AF", "8737": "\\u2221", "8735": "\\u221F", "8894": "\\u22BE", "10653": "\\u299D", "8738": "\\u2222", "197": "\\u00C5", "9084": "\\u237C", "260": "\\u0104", "261": "\\u0105", "120120": "\\uD835\\uDD38", "120146": "\\uD835\\uDD52", "10863": "\\u2A6F", "8776": "\\u2248", "10864": "\\u2A70", "8778": "\\u224A", "8779": "\\u224B", "39": "\\u0027", "8289": "\\u2061", "8776": "\\u2248", "8778": "\\u224A", "197": "\\u00C5", "197": "\\u00C5", "229": "\\u00E5", "229": "\\u00E5", "119964": "\\uD835\\uDC9C", "119990": "\\uD835\\uDCB6", "8788": "\\u2254", "42": "\\u002A", "8776": "\\u2248", "8781": "\\u224D", "195": "\\u00C3", "195": "\\u00C3", "227": "\\u00E3", "227": "\\u00E3", "196": "\\u00C4", "196": "\\u00C4", "228": "\\u00E4", "228": "\\u00E4", "8755": "\\u2233", "10769": "\\u2A11", "8780": "\\u224C", "1014": "\\u03F6", "8245": "\\u2035", "8765": "\\u223D", "8909": "\\u22CD", "8726": "\\u2216", "10983": "\\u2AE7", "8893": "\\u22BD", "8965": "\\u2305", "8966": "\\u2306", "8965": "\\u2305", "9141": "\\u23B5", "9142": "\\u23B6", "8780": "\\u224C", "1041": "\\u0411", "1073": "\\u0431", "8222": "\\u201E", "8757": "\\u2235", "8757": "\\u2235", "8757": "\\u2235", "10672": "\\u29B0", "1014": "\\u03F6", "8492": "\\u212C", "8492": "\\u212C", "914": "\\u0392", "946": "\\u03B2", "8502": "\\u2136", "8812": "\\u226C", "120069": "\\uD835\\uDD05", "120095": "\\uD835\\uDD1F", "8898": "\\u22C2", "9711": "\\u25EF", "8899": "\\u22C3", "10752": "\\u2A00", "10753": "\\u2A01", "10754": "\\u2A02", "10758": "\\u2A06", "9733": "\\u2605", "9661": "\\u25BD", "9651": "\\u25B3", "10756": "\\u2A04", "8897": "\\u22C1", "8896": "\\u22C0", "10509": "\\u290D", "10731": "\\u29EB", "9642": "\\u25AA", "9652": "\\u25B4", "9662": "\\u25BE", "9666": "\\u25C2", "9656": "\\u25B8", "9251": "\\u2423", "9618": "\\u2592", "9617": "\\u2591", "9619": "\\u2593", "9608": "\\u2588", "61": "\\u003D", "8421": "\\u20E5", "8801": "\\u2261", "8421": "\\u20E5", "10989": "\\u2AED", "8976": "\\u2310", "120121": "\\uD835\\uDD39", "120147": "\\uD835\\uDD53", "8869": "\\u22A5", "8869": "\\u22A5", "8904": "\\u22C8", "10697": "\\u29C9", "9488": "\\u2510", "9557": "\\u2555", "9558": "\\u2556", "9559": "\\u2557", "9484": "\\u250C", "9554": "\\u2552", "9555": "\\u2553", "9556": "\\u2554", "9472": "\\u2500", "9552": "\\u2550", "9516": "\\u252C", "9572": "\\u2564", "9573": "\\u2565", "9574": "\\u2566", "9524": "\\u2534", "9575": "\\u2567", "9576": "\\u2568", "9577": "\\u2569", "8863": "\\u229F", "8862": "\\u229E", "8864": "\\u22A0", "9496": "\\u2518", "9563": "\\u255B", "9564": "\\u255C", "9565": "\\u255D", "9492": "\\u2514", "9560": "\\u2558", "9561": "\\u2559", "9562": "\\u255A", "9474": "\\u2502", "9553": "\\u2551", "9532": "\\u253C", "9578": "\\u256A", "9579": "\\u256B", "9580": "\\u256C", "9508": "\\u2524", "9569": "\\u2561", "9570": "\\u2562", "9571": "\\u2563", "9500": "\\u251C", "9566": "\\u255E", "9567": "\\u255F", "9568": "\\u2560", "8245": "\\u2035", "728": "\\u02D8", "728": "\\u02D8", "166": "\\u00A6", "166": "\\u00A6", "119991": "\\uD835\\uDCB7", "8492": "\\u212C", "8271": "\\u204F", "8765": "\\u223D", "8909": "\\u22CD", "10693": "\\u29C5", "92": "\\u005C", "10184": "\\u27C8", "8226": "\\u2022", "8226": "\\u2022", "8782": "\\u224E", "10926": "\\u2AAE", "8783": "\\u224F", "8782": "\\u224E", "8783": "\\u224F", "262": "\\u0106", "263": "\\u0107", "10820": "\\u2A44", "10825": "\\u2A49", "10827": "\\u2A4B", "8745": "\\u2229", "8914": "\\u22D2", "10823": "\\u2A47", "10816": "\\u2A40", "8517": "\\u2145", "8745": "\\u2229", "65024": "\\uFE00", "8257": "\\u2041", "711": "\\u02C7", "8493": "\\u212D", "10829": "\\u2A4D", "268": "\\u010C", "269": "\\u010D", "199": "\\u00C7", "199": "\\u00C7", "231": "\\u00E7", "231": "\\u00E7", "264": "\\u0108", "265": "\\u0109", "8752": "\\u2230", "10828": "\\u2A4C", "10832": "\\u2A50", "266": "\\u010A", "267": "\\u010B", "184": "\\u00B8", "184": "\\u00B8", "184": "\\u00B8", "10674": "\\u29B2", "162": "\\u00A2", "162": "\\u00A2", "183": "\\u00B7", "183": "\\u00B7", "120096": "\\uD835\\uDD20", "8493": "\\u212D", "1063": "\\u0427", "1095": "\\u0447", "10003": "\\u2713", "10003": "\\u2713", "935": "\\u03A7", "967": "\\u03C7", "710": "\\u02C6", "8791": "\\u2257", "8634": "\\u21BA", "8635": "\\u21BB", "8859": "\\u229B", "8858": "\\u229A", "8861": "\\u229D", "8857": "\\u2299", "174": "\\u00AE", "9416": "\\u24C8", "8854": "\\u2296", "8853": "\\u2295", "8855": "\\u2297", "9675": "\\u25CB", "10691": "\\u29C3", "8791": "\\u2257", "10768": "\\u2A10", "10991": "\\u2AEF", "10690": "\\u29C2", "8754": "\\u2232", "8221": "\\u201D", "8217": "\\u2019", "9827": "\\u2663", "9827": "\\u2663", "58": "\\u003A", "8759": "\\u2237", "10868": "\\u2A74", "8788": "\\u2254", "8788": "\\u2254", "44": "\\u002C", "64": "\\u0040", "8705": "\\u2201", "8728": "\\u2218", "8705": "\\u2201", "8450": "\\u2102", "8773": "\\u2245", "10861": "\\u2A6D", "8801": "\\u2261", "8750": "\\u222E", "8751": "\\u222F", "8750": "\\u222E", "120148": "\\uD835\\uDD54", "8450": "\\u2102", "8720": "\\u2210", "8720": "\\u2210", "169": "\\u00A9", "169": "\\u00A9", "169": "\\u00A9", "169": "\\u00A9", "8471": "\\u2117", "8755": "\\u2233", "8629": "\\u21B5", "10007": "\\u2717", "10799": "\\u2A2F", "119966": "\\uD835\\uDC9E", "119992": "\\uD835\\uDCB8", "10959": "\\u2ACF", "10961": "\\u2AD1", "10960": "\\u2AD0", "10962": "\\u2AD2", "8943": "\\u22EF", "10552": "\\u2938", "10549": "\\u2935", "8926": "\\u22DE", "8927": "\\u22DF", "8630": "\\u21B6", "10557": "\\u293D", "10824": "\\u2A48", "10822": "\\u2A46", "8781": "\\u224D", "8746": "\\u222A", "8915": "\\u22D3", "10826": "\\u2A4A", "8845": "\\u228D", "10821": "\\u2A45", "8746": "\\u222A", "65024": "\\uFE00", "8631": "\\u21B7", "10556": "\\u293C", "8926": "\\u22DE", "8927": "\\u22DF", "8910": "\\u22CE", "8911": "\\u22CF", "164": "\\u00A4", "164": "\\u00A4", "8630": "\\u21B6", "8631": "\\u21B7", "8910": "\\u22CE", "8911": "\\u22CF", "8754": "\\u2232", "8753": "\\u2231", "9005": "\\u232D", "8224": "\\u2020", "8225": "\\u2021", "8504": "\\u2138", "8595": "\\u2193", "8609": "\\u21A1", "8659": "\\u21D3", "8208": "\\u2010", "10980": "\\u2AE4", "8867": "\\u22A3", "10511": "\\u290F", "733": "\\u02DD", "270": "\\u010E", "271": "\\u010F", "1044": "\\u0414", "1076": "\\u0434", "8225": "\\u2021", "8650": "\\u21CA", "8517": "\\u2145", "8518": "\\u2146", "10513": "\\u2911", "10871": "\\u2A77", "176": "\\u00B0", "176": "\\u00B0", "8711": "\\u2207", "916": "\\u0394", "948": "\\u03B4", "10673": "\\u29B1", "10623": "\\u297F", "120071": "\\uD835\\uDD07", "120097": "\\uD835\\uDD21", "10597": "\\u2965", "8643": "\\u21C3", "8642": "\\u21C2", "180": "\\u00B4", "729": "\\u02D9", "733": "\\u02DD", "96": "\\u0060", "732": "\\u02DC", "8900": "\\u22C4", "8900": "\\u22C4", "8900": "\\u22C4", "9830": "\\u2666", "9830": "\\u2666", "168": "\\u00A8", "8518": "\\u2146", "989": "\\u03DD", "8946": "\\u22F2", "247": "\\u00F7", "247": "\\u00F7", "247": "\\u00F7", "8903": "\\u22C7", "8903": "\\u22C7", "1026": "\\u0402", "1106": "\\u0452", "8990": "\\u231E", "8973": "\\u230D", "36": "\\u0024", "120123": "\\uD835\\uDD3B", "120149": "\\uD835\\uDD55", "168": "\\u00A8", "729": "\\u02D9", "8412": "\\u20DC", "8784": "\\u2250", "8785": "\\u2251", "8784": "\\u2250", "8760": "\\u2238", "8724": "\\u2214", "8865": "\\u22A1", "8966": "\\u2306", "8751": "\\u222F", "168": "\\u00A8", "8659": "\\u21D3", "8656": "\\u21D0", "8660": "\\u21D4", "10980": "\\u2AE4", "10232": "\\u27F8", "10234": "\\u27FA", "10233": "\\u27F9", "8658": "\\u21D2", "8872": "\\u22A8", "8657": "\\u21D1", "8661": "\\u21D5", "8741": "\\u2225", "10515": "\\u2913", "8595": "\\u2193", "8595": "\\u2193", "8659": "\\u21D3", "8693": "\\u21F5", "785": "\\u0311", "8650": "\\u21CA", "8643": "\\u21C3", "8642": "\\u21C2", "10576": "\\u2950", "10590": "\\u295E", "10582": "\\u2956", "8637": "\\u21BD", "10591": "\\u295F", "10583": "\\u2957", "8641": "\\u21C1", "8615": "\\u21A7", "8868": "\\u22A4", "10512": "\\u2910", "8991": "\\u231F", "8972": "\\u230C", "119967": "\\uD835\\uDC9F", "119993": "\\uD835\\uDCB9", "1029": "\\u0405", "1109": "\\u0455", "10742": "\\u29F6", "272": "\\u0110", "273": "\\u0111", "8945": "\\u22F1", "9663": "\\u25BF", "9662": "\\u25BE", "8693": "\\u21F5", "10607": "\\u296F", "10662": "\\u29A6", "1039": "\\u040F", "1119": "\\u045F", "10239": "\\u27FF", "201": "\\u00C9", "201": "\\u00C9", "233": "\\u00E9", "233": "\\u00E9", "10862": "\\u2A6E", "282": "\\u011A", "283": "\\u011B", "202": "\\u00CA", "202": "\\u00CA", "234": "\\u00EA", "234": "\\u00EA", "8790": "\\u2256", "8789": "\\u2255", "1069": "\\u042D", "1101": "\\u044D", "10871": "\\u2A77", "278": "\\u0116", "279": "\\u0117", "8785": "\\u2251", "8519": "\\u2147", "8786": "\\u2252", "120072": "\\uD835\\uDD08", "120098": "\\uD835\\uDD22", "10906": "\\u2A9A", "200": "\\u00C8", "200": "\\u00C8", "232": "\\u00E8", "232": "\\u00E8", "10902": "\\u2A96", "10904": "\\u2A98", "10905": "\\u2A99", "8712": "\\u2208", "9191": "\\u23E7", "8467": "\\u2113", "10901": "\\u2A95", "10903": "\\u2A97", "274": "\\u0112", "275": "\\u0113", "8709": "\\u2205", "8709": "\\u2205", "9723": "\\u25FB", "8709": "\\u2205", "9643": "\\u25AB", "8196": "\\u2004", "8197": "\\u2005", "8195": "\\u2003", "330": "\\u014A", "331": "\\u014B", "8194": "\\u2002", "280": "\\u0118", "281": "\\u0119", "120124": "\\uD835\\uDD3C", "120150": "\\uD835\\uDD56", "8917": "\\u22D5", "10723": "\\u29E3", "10865": "\\u2A71", "949": "\\u03B5", "917": "\\u0395", "949": "\\u03B5", "1013": "\\u03F5", "8790": "\\u2256", "8789": "\\u2255", "8770": "\\u2242", "10902": "\\u2A96", "10901": "\\u2A95", "10869": "\\u2A75", "61": "\\u003D", "8770": "\\u2242", "8799": "\\u225F", "8652": "\\u21CC", "8801": "\\u2261", "10872": "\\u2A78", "10725": "\\u29E5", "10609": "\\u2971", "8787": "\\u2253", "8495": "\\u212F", "8496": "\\u2130", "8784": "\\u2250", "10867": "\\u2A73", "8770": "\\u2242", "919": "\\u0397", "951": "\\u03B7", "208": "\\u00D0", "208": "\\u00D0", "240": "\\u00F0", "240": "\\u00F0", "203": "\\u00CB", "203": "\\u00CB", "235": "\\u00EB", "235": "\\u00EB", "8364": "\\u20AC", "33": "\\u0021", "8707": "\\u2203", "8707": "\\u2203", "8496": "\\u2130", "8519": "\\u2147", "8519": "\\u2147", "8786": "\\u2252", "1060": "\\u0424", "1092": "\\u0444", "9792": "\\u2640", "64259": "\\uFB03", "64256": "\\uFB00", "64260": "\\uFB04", "120073": "\\uD835\\uDD09", "120099": "\\uD835\\uDD23", "64257": "\\uFB01", "9724": "\\u25FC", "9642": "\\u25AA", "102": "\\u0066", "106": "\\u006A", "9837": "\\u266D", "64258": "\\uFB02", "9649": "\\u25B1", "402": "\\u0192", "120125": "\\uD835\\uDD3D", "120151": "\\uD835\\uDD57", "8704": "\\u2200", "8704": "\\u2200", "8916": "\\u22D4", "10969": "\\u2AD9", "8497": "\\u2131", "10765": "\\u2A0D", "189": "\\u00BD", "189": "\\u00BD", "8531": "\\u2153", "188": "\\u00BC", "188": "\\u00BC", "8533": "\\u2155", "8537": "\\u2159", "8539": "\\u215B", "8532": "\\u2154", "8534": "\\u2156", "190": "\\u00BE", "190": "\\u00BE", "8535": "\\u2157", "8540": "\\u215C", "8536": "\\u2158", "8538": "\\u215A", "8541": "\\u215D", "8542": "\\u215E", "8260": "\\u2044", "8994": "\\u2322", "119995": "\\uD835\\uDCBB", "8497": "\\u2131", "501": "\\u01F5", "915": "\\u0393", "947": "\\u03B3", "988": "\\u03DC", "989": "\\u03DD", "10886": "\\u2A86", "286": "\\u011E", "287": "\\u011F", "290": "\\u0122", "284": "\\u011C", "285": "\\u011D", "1043": "\\u0413", "1075": "\\u0433", "288": "\\u0120", "289": "\\u0121", "8805": "\\u2265", "8807": "\\u2267", "10892": "\\u2A8C", "8923": "\\u22DB", "8805": "\\u2265", "8807": "\\u2267", "10878": "\\u2A7E", "10921": "\\u2AA9", "10878": "\\u2A7E", "10880": "\\u2A80", "10882": "\\u2A82", "10884": "\\u2A84", "8923": "\\u22DB", "65024": "\\uFE00", "10900": "\\u2A94", "120074": "\\uD835\\uDD0A", "120100": "\\uD835\\uDD24", "8811": "\\u226B", "8921": "\\u22D9", "8921": "\\u22D9", "8503": "\\u2137", "1027": "\\u0403", "1107": "\\u0453", "10917": "\\u2AA5", "8823": "\\u2277", "10898": "\\u2A92", "10916": "\\u2AA4", "10890": "\\u2A8A", "10890": "\\u2A8A", "10888": "\\u2A88", "8809": "\\u2269", "10888": "\\u2A88", "8809": "\\u2269", "8935": "\\u22E7", "120126": "\\uD835\\uDD3E", "120152": "\\uD835\\uDD58", "96": "\\u0060", "8805": "\\u2265", "8923": "\\u22DB", "8807": "\\u2267", "10914": "\\u2AA2", "8823": "\\u2277", "10878": "\\u2A7E", "8819": "\\u2273", "119970": "\\uD835\\uDCA2", "8458": "\\u210A", "8819": "\\u2273", "10894": "\\u2A8E", "10896": "\\u2A90", "10919": "\\u2AA7", "10874": "\\u2A7A", "62": "\\u003E", "62": "\\u003E", "62": "\\u003E", "62": "\\u003E", "8811": "\\u226B", "8919": "\\u22D7", "10645": "\\u2995", "10876": "\\u2A7C", "10886": "\\u2A86", "10616": "\\u2978", "8919": "\\u22D7", "8923": "\\u22DB", "10892": "\\u2A8C", "8823": "\\u2277", "8819": "\\u2273", "8809": "\\u2269", "65024": "\\uFE00", "8809": "\\u2269", "65024": "\\uFE00", "711": "\\u02C7", "8202": "\\u200A", "189": "\\u00BD", "8459": "\\u210B", "1066": "\\u042A", "1098": "\\u044A", "10568": "\\u2948", "8596": "\\u2194", "8660": "\\u21D4", "8621": "\\u21AD", "94": "\\u005E", "8463": "\\u210F", "292": "\\u0124", "293": "\\u0125", "9829": "\\u2665", "9829": "\\u2665", "8230": "\\u2026", "8889": "\\u22B9", "120101": "\\uD835\\uDD25", "8460": "\\u210C", "8459": "\\u210B", "10533": "\\u2925", "10534": "\\u2926", "8703": "\\u21FF", "8763": "\\u223B", "8617": "\\u21A9", "8618": "\\u21AA", "120153": "\\uD835\\uDD59", "8461": "\\u210D", "8213": "\\u2015", "9472": "\\u2500", "119997": "\\uD835\\uDCBD", "8459": "\\u210B", "8463": "\\u210F", "294": "\\u0126", "295": "\\u0127", "8782": "\\u224E", "8783": "\\u224F", "8259": "\\u2043", "8208": "\\u2010", "205": "\\u00CD", "205": "\\u00CD", "237": "\\u00ED", "237": "\\u00ED", "8291": "\\u2063", "206": "\\u00CE", "206": "\\u00CE", "238": "\\u00EE", "238": "\\u00EE", "1048": "\\u0418", "1080": "\\u0438", "304": "\\u0130", "1045": "\\u0415", "1077": "\\u0435", "161": "\\u00A1", "161": "\\u00A1", "8660": "\\u21D4", "120102": "\\uD835\\uDD26", "8465": "\\u2111", "204": "\\u00CC", "204": "\\u00CC", "236": "\\u00EC", "236": "\\u00EC", "8520": "\\u2148", "10764": "\\u2A0C", "8749": "\\u222D", "10716": "\\u29DC", "8489": "\\u2129", "306": "\\u0132", "307": "\\u0133", "298": "\\u012A", "299": "\\u012B", "8465": "\\u2111", "8520": "\\u2148", "8464": "\\u2110", "8465": "\\u2111", "305": "\\u0131", "8465": "\\u2111", "8887": "\\u22B7", "437": "\\u01B5", "8658": "\\u21D2", "8453": "\\u2105", "8712": "\\u2208", "8734": "\\u221E", "10717": "\\u29DD", "305": "\\u0131", "8890": "\\u22BA", "8747": "\\u222B", "8748": "\\u222C", "8484": "\\u2124", "8747": "\\u222B", "8890": "\\u22BA", "8898": "\\u22C2", "10775": "\\u2A17", "10812": "\\u2A3C", "8291": "\\u2063", "8290": "\\u2062", "1025": "\\u0401", "1105": "\\u0451", "302": "\\u012E", "303": "\\u012F", "120128": "\\uD835\\uDD40", "120154": "\\uD835\\uDD5A", "921": "\\u0399", "953": "\\u03B9", "10812": "\\u2A3C", "191": "\\u00BF", "191": "\\u00BF", "119998": "\\uD835\\uDCBE", "8464": "\\u2110", "8712": "\\u2208", "8949": "\\u22F5", "8953": "\\u22F9", "8948": "\\u22F4", "8947": "\\u22F3", "8712": "\\u2208", "8290": "\\u2062", "296": "\\u0128", "297": "\\u0129", "1030": "\\u0406", "1110": "\\u0456", "207": "\\u00CF", "207": "\\u00CF", "239": "\\u00EF", "239": "\\u00EF", "308": "\\u0134", "309": "\\u0135", "1049": "\\u0419", "1081": "\\u0439", "120077": "\\uD835\\uDD0D", "120103": "\\uD835\\uDD27", "567": "\\u0237", "120129": "\\uD835\\uDD41", "120155": "\\uD835\\uDD5B", "119973": "\\uD835\\uDCA5", "119999": "\\uD835\\uDCBF", "1032": "\\u0408", "1112": "\\u0458", "1028": "\\u0404", "1108": "\\u0454", "922": "\\u039A", "954": "\\u03BA", "1008": "\\u03F0", "310": "\\u0136", "311": "\\u0137", "1050": "\\u041A", "1082": "\\u043A", "120078": "\\uD835\\uDD0E", "120104": "\\uD835\\uDD28", "312": "\\u0138", "1061": "\\u0425", "1093": "\\u0445", "1036": "\\u040C", "1116": "\\u045C", "120130": "\\uD835\\uDD42", "120156": "\\uD835\\uDD5C", "119974": "\\uD835\\uDCA6", "120000": "\\uD835\\uDCC0", "8666": "\\u21DA", "313": "\\u0139", "314": "\\u013A", "10676": "\\u29B4", "8466": "\\u2112", "923": "\\u039B", "955": "\\u03BB", "10216": "\\u27E8", "10218": "\\u27EA", "10641": "\\u2991", "10216": "\\u27E8", "10885": "\\u2A85", "8466": "\\u2112", "171": "\\u00AB", "171": "\\u00AB", "8676": "\\u21E4", "10527": "\\u291F", "8592": "\\u2190", "8606": "\\u219E", "8656": "\\u21D0", "10525": "\\u291D", "8617": "\\u21A9", "8619": "\\u21AB", "10553": "\\u2939", "10611": "\\u2973", "8610": "\\u21A2", "10521": "\\u2919", "10523": "\\u291B", "10923": "\\u2AAB", "10925": "\\u2AAD", "10925": "\\u2AAD", "65024": "\\uFE00", "10508": "\\u290C", "10510": "\\u290E", "10098": "\\u2772", "123": "\\u007B", "91": "\\u005B", "10635": "\\u298B", "10639": "\\u298F", "10637": "\\u298D", "317": "\\u013D", "318": "\\u013E", "315": "\\u013B", "316": "\\u013C", "8968": "\\u2308", "123": "\\u007B", "1051": "\\u041B", "1083": "\\u043B", "10550": "\\u2936", "8220": "\\u201C", "8222": "\\u201E", "10599": "\\u2967", "10571": "\\u294B", "8626": "\\u21B2", "8804": "\\u2264", "8806": "\\u2266", "10216": "\\u27E8", "8676": "\\u21E4", "8592": "\\u2190", "8592": "\\u2190", "8656": "\\u21D0", "8646": "\\u21C6", "8610": "\\u21A2", "8968": "\\u2308", "10214": "\\u27E6", "10593": "\\u2961", "10585": "\\u2959", "8643": "\\u21C3", "8970": "\\u230A", "8637": "\\u21BD", "8636": "\\u21BC", "8647": "\\u21C7", "8596": "\\u2194", "8596": "\\u2194", "8660": "\\u21D4", "8646": "\\u21C6", "8651": "\\u21CB", "8621": "\\u21AD", "10574": "\\u294E", "8612": "\\u21A4", "8867": "\\u22A3", "10586": "\\u295A", "8907": "\\u22CB", "10703": "\\u29CF", "8882": "\\u22B2", "8884": "\\u22B4", "10577": "\\u2951", "10592": "\\u2960", "10584": "\\u2958", "8639": "\\u21BF", "10578": "\\u2952", "8636": "\\u21BC", "10891": "\\u2A8B", "8922": "\\u22DA", "8804": "\\u2264", "8806": "\\u2266", "10877": "\\u2A7D", "10920": "\\u2AA8", "10877": "\\u2A7D", "10879": "\\u2A7F", "10881": "\\u2A81", "10883": "\\u2A83", "8922": "\\u22DA", "65024": "\\uFE00", "10899": "\\u2A93", "10885": "\\u2A85", "8918": "\\u22D6", "8922": "\\u22DA", "10891": "\\u2A8B", "8922": "\\u22DA", "8806": "\\u2266", "8822": "\\u2276", "8822": "\\u2276", "10913": "\\u2AA1", "8818": "\\u2272", "10877": "\\u2A7D", "8818": "\\u2272", "10620": "\\u297C", "8970": "\\u230A", "120079": "\\uD835\\uDD0F", "120105": "\\uD835\\uDD29", "8822": "\\u2276", "10897": "\\u2A91", "10594": "\\u2962", "8637": "\\u21BD", "8636": "\\u21BC", "10602": "\\u296A", "9604": "\\u2584", "1033": "\\u0409", "1113": "\\u0459", "8647": "\\u21C7", "8810": "\\u226A", "8920": "\\u22D8", "8990": "\\u231E", "8666": "\\u21DA", "10603": "\\u296B", "9722": "\\u25FA", "319": "\\u013F", "320": "\\u0140", "9136": "\\u23B0", "9136": "\\u23B0", "10889": "\\u2A89", "10889": "\\u2A89", "10887": "\\u2A87", "8808": "\\u2268", "10887": "\\u2A87", "8808": "\\u2268", "8934": "\\u22E6", "10220": "\\u27EC", "8701": "\\u21FD", "10214": "\\u27E6", "10229": "\\u27F5", "10229": "\\u27F5", "10232": "\\u27F8", "10231": "\\u27F7", "10231": "\\u27F7", "10234": "\\u27FA", "10236": "\\u27FC", "10230": "\\u27F6", "10230": "\\u27F6", "10233": "\\u27F9", "8619": "\\u21AB", "8620": "\\u21AC", "10629": "\\u2985", "120131": "\\uD835\\uDD43", "120157": "\\uD835\\uDD5D", "10797": "\\u2A2D", "10804": "\\u2A34", "8727": "\\u2217", "95": "\\u005F", "8601": "\\u2199", "8600": "\\u2198", "9674": "\\u25CA", "9674": "\\u25CA", "10731": "\\u29EB", "40": "\\u0028", "10643": "\\u2993", "8646": "\\u21C6", "8991": "\\u231F", "8651": "\\u21CB", "10605": "\\u296D", "8206": "\\u200E", "8895": "\\u22BF", "8249": "\\u2039", "120001": "\\uD835\\uDCC1", "8466": "\\u2112", "8624": "\\u21B0", "8624": "\\u21B0", "8818": "\\u2272", "10893": "\\u2A8D", "10895": "\\u2A8F", "91": "\\u005B", "8216": "\\u2018", "8218": "\\u201A", "321": "\\u0141", "322": "\\u0142", "10918": "\\u2AA6", "10873": "\\u2A79", "60": "\\u003C", "60": "\\u003C", "60": "\\u003C", "60": "\\u003C", "8810": "\\u226A", "8918": "\\u22D6", "8907": "\\u22CB", "8905": "\\u22C9", "10614": "\\u2976", "10875": "\\u2A7B", "9667": "\\u25C3", "8884": "\\u22B4", "9666": "\\u25C2", "10646": "\\u2996", "10570": "\\u294A", "10598": "\\u2966", "8808": "\\u2268", "65024": "\\uFE00", "8808": "\\u2268", "65024": "\\uFE00", "175": "\\u00AF", "175": "\\u00AF", "9794": "\\u2642", "10016": "\\u2720", "10016": "\\u2720", "10501": "\\u2905", "8614": "\\u21A6", "8614": "\\u21A6", "8615": "\\u21A7", "8612": "\\u21A4", "8613": "\\u21A5", "9646": "\\u25AE", "10793": "\\u2A29", "1052": "\\u041C", "1084": "\\u043C", "8212": "\\u2014", "8762": "\\u223A", "8737": "\\u2221", "8287": "\\u205F", "8499": "\\u2133", "120080": "\\uD835\\uDD10", "120106": "\\uD835\\uDD2A", "8487": "\\u2127", "181": "\\u00B5", "181": "\\u00B5", "42": "\\u002A", "10992": "\\u2AF0", "8739": "\\u2223", "183": "\\u00B7", "183": "\\u00B7", "8863": "\\u229F", "8722": "\\u2212", "8760": "\\u2238", "10794": "\\u2A2A", "8723": "\\u2213", "10971": "\\u2ADB", "8230": "\\u2026", "8723": "\\u2213", "8871": "\\u22A7", "120132": "\\uD835\\uDD44", "120158": "\\uD835\\uDD5E", "8723": "\\u2213", "120002": "\\uD835\\uDCC2", "8499": "\\u2133", "8766": "\\u223E", "924": "\\u039C", "956": "\\u03BC", "8888": "\\u22B8", "8888": "\\u22B8", "8711": "\\u2207", "323": "\\u0143", "324": "\\u0144", "8736": "\\u2220", "8402": "\\u20D2", "8777": "\\u2249", "10864": "\\u2A70", "824": "\\u0338", "8779": "\\u224B", "824": "\\u0338", "329": "\\u0149", "8777": "\\u2249", "9838": "\\u266E", "8469": "\\u2115", "9838": "\\u266E", "160": "\\u00A0", "160": "\\u00A0", "8782": "\\u224E", "824": "\\u0338", "8783": "\\u224F", "824": "\\u0338", "10819": "\\u2A43", "327": "\\u0147", "328": "\\u0148", "325": "\\u0145", "326": "\\u0146", "8775": "\\u2247", "10861": "\\u2A6D", "824": "\\u0338", "10818": "\\u2A42", "1053": "\\u041D", "1085": "\\u043D", "8211": "\\u2013", "10532": "\\u2924", "8599": "\\u2197", "8663": "\\u21D7", "8599": "\\u2197", "8800": "\\u2260", "8784": "\\u2250", "824": "\\u0338", "8203": "\\u200B", "8203": "\\u200B", "8203": "\\u200B", "8203": "\\u200B", "8802": "\\u2262", "10536": "\\u2928", "8770": "\\u2242", "824": "\\u0338", "8811": "\\u226B", "8810": "\\u226A", "10": "\\u000A", "8708": "\\u2204", "8708": "\\u2204", "120081": "\\uD835\\uDD11", "120107": "\\uD835\\uDD2B", "8807": "\\u2267", "824": "\\u0338", "8817": "\\u2271", "8817": "\\u2271", "8807": "\\u2267", "824": "\\u0338", "10878": "\\u2A7E", "824": "\\u0338", "10878": "\\u2A7E", "824": "\\u0338", "8921": "\\u22D9", "824": "\\u0338", "8821": "\\u2275", "8811": "\\u226B", "8402": "\\u20D2", "8815": "\\u226F", "8815": "\\u226F", "8811": "\\u226B", "824": "\\u0338", "8622": "\\u21AE", "8654": "\\u21CE", "10994": "\\u2AF2", "8715": "\\u220B", "8956": "\\u22FC", "8954": "\\u22FA", "8715": "\\u220B", "1034": "\\u040A", "1114": "\\u045A", "8602": "\\u219A", "8653": "\\u21CD", "8229": "\\u2025", "8806": "\\u2266", "824": "\\u0338", "8816": "\\u2270", "8602": "\\u219A", "8653": "\\u21CD", "8622": "\\u21AE", "8654": "\\u21CE", "8816": "\\u2270", "8806": "\\u2266", "824": "\\u0338", "10877": "\\u2A7D", "824": "\\u0338", "10877": "\\u2A7D", "824": "\\u0338", "8814": "\\u226E", "8920": "\\u22D8", "824": "\\u0338", "8820": "\\u2274", "8810": "\\u226A", "8402": "\\u20D2", "8814": "\\u226E", "8938": "\\u22EA", "8940": "\\u22EC", "8810": "\\u226A", "824": "\\u0338", "8740": "\\u2224", "8288": "\\u2060", "160": "\\u00A0", "120159": "\\uD835\\uDD5F", "8469": "\\u2115", "10988": "\\u2AEC", "172": "\\u00AC", "172": "\\u00AC", "8802": "\\u2262", "8813": "\\u226D", "8742": "\\u2226", "8713": "\\u2209", "8800": "\\u2260", "8770": "\\u2242", "824": "\\u0338", "8708": "\\u2204", "8815": "\\u226F", "8817": "\\u2271", "8807": "\\u2267", "824": "\\u0338", "8811": "\\u226B", "824": "\\u0338", "8825": "\\u2279", "10878": "\\u2A7E", "824": "\\u0338", "8821": "\\u2275", "8782": "\\u224E", "824": "\\u0338", "8783": "\\u224F", "824": "\\u0338", "8713": "\\u2209", "8949": "\\u22F5", "824": "\\u0338", "8953": "\\u22F9", "824": "\\u0338", "8713": "\\u2209", "8951": "\\u22F7", "8950": "\\u22F6", "10703": "\\u29CF", "824": "\\u0338", "8938": "\\u22EA", "8940": "\\u22EC", "8814": "\\u226E", "8816": "\\u2270", "8824": "\\u2278", "8810": "\\u226A", "824": "\\u0338", "10877": "\\u2A7D", "824": "\\u0338", "8820": "\\u2274", "10914": "\\u2AA2", "824": "\\u0338", "10913": "\\u2AA1", "824": "\\u0338", "8716": "\\u220C", "8716": "\\u220C", "8958": "\\u22FE", "8957": "\\u22FD", "8832": "\\u2280", "10927": "\\u2AAF", "824": "\\u0338", "8928": "\\u22E0", "8716": "\\u220C", "10704": "\\u29D0", "824": "\\u0338", "8939": "\\u22EB", "8941": "\\u22ED", "8847": "\\u228F", "824": "\\u0338", "8930": "\\u22E2", "8848": "\\u2290", "824": "\\u0338", "8931": "\\u22E3", "8834": "\\u2282", "8402": "\\u20D2", "8840": "\\u2288", "8833": "\\u2281", "10928": "\\u2AB0", "824": "\\u0338", "8929": "\\u22E1", "8831": "\\u227F", "824": "\\u0338", "8835": "\\u2283", "8402": "\\u20D2", "8841": "\\u2289", "8769": "\\u2241", "8772": "\\u2244", "8775": "\\u2247", "8777": "\\u2249", "8740": "\\u2224", "8742": "\\u2226", "8742": "\\u2226", "11005": "\\u2AFD", "8421": "\\u20E5", "8706": "\\u2202", "824": "\\u0338", "10772": "\\u2A14", "8832": "\\u2280", "8928": "\\u22E0", "8832": "\\u2280", "10927": "\\u2AAF", "824": "\\u0338", "10927": "\\u2AAF", "824": "\\u0338", "10547": "\\u2933", "824": "\\u0338", "8603": "\\u219B", "8655": "\\u21CF", "8605": "\\u219D", "824": "\\u0338", "8603": "\\u219B", "8655": "\\u21CF", "8939": "\\u22EB", "8941": "\\u22ED", "8833": "\\u2281", "8929": "\\u22E1", "10928": "\\u2AB0", "824": "\\u0338", "119977": "\\uD835\\uDCA9", "120003": "\\uD835\\uDCC3", "8740": "\\u2224", "8742": "\\u2226", "8769": "\\u2241", "8772": "\\u2244", "8772": "\\u2244", "8740": "\\u2224", "8742": "\\u2226", "8930": "\\u22E2", "8931": "\\u22E3", "8836": "\\u2284", "10949": "\\u2AC5", "824": "\\u0338", "8840": "\\u2288", "8834": "\\u2282", "8402": "\\u20D2", "8840": "\\u2288", "10949": "\\u2AC5", "824": "\\u0338", "8833": "\\u2281", "10928": "\\u2AB0", "824": "\\u0338", "8837": "\\u2285", "10950": "\\u2AC6", "824": "\\u0338", "8841": "\\u2289", "8835": "\\u2283", "8402": "\\u20D2", "8841": "\\u2289", "10950": "\\u2AC6", "824": "\\u0338", "8825": "\\u2279", "209": "\\u00D1", "209": "\\u00D1", "241": "\\u00F1", "241": "\\u00F1", "8824": "\\u2278", "8938": "\\u22EA", "8940": "\\u22EC", "8939": "\\u22EB", "8941": "\\u22ED", "925": "\\u039D", "957": "\\u03BD", "35": "\\u0023", "8470": "\\u2116", "8199": "\\u2007", "8781": "\\u224D", "8402": "\\u20D2", "8876": "\\u22AC", "8877": "\\u22AD", "8878": "\\u22AE", "8879": "\\u22AF", "8805": "\\u2265", "8402": "\\u20D2", "62": "\\u003E", "8402": "\\u20D2", "10500": "\\u2904", "10718": "\\u29DE", "10498": "\\u2902", "8804": "\\u2264", "8402": "\\u20D2", "60": "\\u003C", "8402": "\\u20D2", "8884": "\\u22B4", "8402": "\\u20D2", "10499": "\\u2903", "8885": "\\u22B5", "8402": "\\u20D2", "8764": "\\u223C", "8402": "\\u20D2", "10531": "\\u2923", "8598": "\\u2196", "8662": "\\u21D6", "8598": "\\u2196", "10535": "\\u2927", "211": "\\u00D3", "211": "\\u00D3", "243": "\\u00F3", "243": "\\u00F3", "8859": "\\u229B", "212": "\\u00D4", "212": "\\u00D4", "244": "\\u00F4", "244": "\\u00F4", "8858": "\\u229A", "1054": "\\u041E", "1086": "\\u043E", "8861": "\\u229D", "336": "\\u0150", "337": "\\u0151", "10808": "\\u2A38", "8857": "\\u2299", "10684": "\\u29BC", "338": "\\u0152", "339": "\\u0153", "10687": "\\u29BF", "120082": "\\uD835\\uDD12", "120108": "\\uD835\\uDD2C", "731": "\\u02DB", "210": "\\u00D2", "210": "\\u00D2", "242": "\\u00F2", "242": "\\u00F2", "10689": "\\u29C1", "10677": "\\u29B5", "937": "\\u03A9", "8750": "\\u222E", "8634": "\\u21BA", "10686": "\\u29BE", "10683": "\\u29BB", "8254": "\\u203E", "10688": "\\u29C0", "332": "\\u014C", "333": "\\u014D", "937": "\\u03A9", "969": "\\u03C9", "927": "\\u039F", "959": "\\u03BF", "10678": "\\u29B6", "8854": "\\u2296", "120134": "\\uD835\\uDD46", "120160": "\\uD835\\uDD60", "10679": "\\u29B7", "8220": "\\u201C", "8216": "\\u2018", "10681": "\\u29B9", "8853": "\\u2295", "8635": "\\u21BB", "10836": "\\u2A54", "8744": "\\u2228", "10845": "\\u2A5D", "8500": "\\u2134", "8500": "\\u2134", "170": "\\u00AA", "170": "\\u00AA", "186": "\\u00BA", "186": "\\u00BA", "8886": "\\u22B6", "10838": "\\u2A56", "10839": "\\u2A57", "10843": "\\u2A5B", "9416": "\\u24C8", "119978": "\\uD835\\uDCAA", "8500": "\\u2134", "216": "\\u00D8", "216": "\\u00D8", "248": "\\u00F8", "248": "\\u00F8", "8856": "\\u2298", "213": "\\u00D5", "213": "\\u00D5", "245": "\\u00F5", "245": "\\u00F5", "10806": "\\u2A36", "10807": "\\u2A37", "8855": "\\u2297", "214": "\\u00D6", "214": "\\u00D6", "246": "\\u00F6", "246": "\\u00F6", "9021": "\\u233D", "8254": "\\u203E", "9182": "\\u23DE", "9140": "\\u23B4", "9180": "\\u23DC", "182": "\\u00B6", "182": "\\u00B6", "8741": "\\u2225", "8741": "\\u2225", "10995": "\\u2AF3", "11005": "\\u2AFD", "8706": "\\u2202", "8706": "\\u2202", "1055": "\\u041F", "1087": "\\u043F", "37": "\\u0025", "46": "\\u002E", "8240": "\\u2030", "8869": "\\u22A5", "8241": "\\u2031", "120083": "\\uD835\\uDD13", "120109": "\\uD835\\uDD2D", "934": "\\u03A6", "966": "\\u03C6", "981": "\\u03D5", "8499": "\\u2133", "9742": "\\u260E", "928": "\\u03A0", "960": "\\u03C0", "8916": "\\u22D4", "982": "\\u03D6", "8463": "\\u210F", "8462": "\\u210E", "8463": "\\u210F", "10787": "\\u2A23", "8862": "\\u229E", "10786": "\\u2A22", "43": "\\u002B", "8724": "\\u2214", "10789": "\\u2A25", "10866": "\\u2A72", "177": "\\u00B1", "177": "\\u00B1", "177": "\\u00B1", "10790": "\\u2A26", "10791": "\\u2A27", "177": "\\u00B1", "8460": "\\u210C", "10773": "\\u2A15", "120161": "\\uD835\\uDD61", "8473": "\\u2119", "163": "\\u00A3", "163": "\\u00A3", "10935": "\\u2AB7", "10939": "\\u2ABB", "8826": "\\u227A", "8828": "\\u227C", "10935": "\\u2AB7", "8826": "\\u227A", "8828": "\\u227C", "8826": "\\u227A", "10927": "\\u2AAF", "8828": "\\u227C", "8830": "\\u227E", "10927": "\\u2AAF", "10937": "\\u2AB9", "10933": "\\u2AB5", "8936": "\\u22E8", "10927": "\\u2AAF", "10931": "\\u2AB3", "8830": "\\u227E", "8242": "\\u2032", "8243": "\\u2033", "8473": "\\u2119", "10937": "\\u2AB9", "10933": "\\u2AB5", "8936": "\\u22E8", "8719": "\\u220F", "8719": "\\u220F", "9006": "\\u232E", "8978": "\\u2312", "8979": "\\u2313", "8733": "\\u221D", "8733": "\\u221D", "8759": "\\u2237", "8733": "\\u221D", "8830": "\\u227E", "8880": "\\u22B0", "119979": "\\uD835\\uDCAB", "120005": "\\uD835\\uDCC5", "936": "\\u03A8", "968": "\\u03C8", "8200": "\\u2008", "120084": "\\uD835\\uDD14", "120110": "\\uD835\\uDD2E", "10764": "\\u2A0C", "120162": "\\uD835\\uDD62", "8474": "\\u211A", "8279": "\\u2057", "119980": "\\uD835\\uDCAC", "120006": "\\uD835\\uDCC6", "8461": "\\u210D", "10774": "\\u2A16", "63": "\\u003F", "8799": "\\u225F", "34": "\\u0022", "34": "\\u0022", "34": "\\u0022", "34": "\\u0022", "8667": "\\u21DB", "8765": "\\u223D", "817": "\\u0331", "340": "\\u0154", "341": "\\u0155", "8730": "\\u221A", "10675": "\\u29B3", "10217": "\\u27E9", "10219": "\\u27EB", "10642": "\\u2992", "10661": "\\u29A5", "10217": "\\u27E9", "187": "\\u00BB", "187": "\\u00BB", "10613": "\\u2975", "8677": "\\u21E5", "10528": "\\u2920", "10547": "\\u2933", "8594": "\\u2192", "8608": "\\u21A0", "8658": "\\u21D2", "10526": "\\u291E", "8618": "\\u21AA", "8620": "\\u21AC", "10565": "\\u2945", "10612": "\\u2974", "10518": "\\u2916", "8611": "\\u21A3", "8605": "\\u219D", "10522": "\\u291A", "10524": "\\u291C", "8758": "\\u2236", "8474": "\\u211A", "10509": "\\u290D", "10511": "\\u290F", "10512": "\\u2910", "10099": "\\u2773", "125": "\\u007D", "93": "\\u005D", "10636": "\\u298C", "10638": "\\u298E", "10640": "\\u2990", "344": "\\u0158", "345": "\\u0159", "342": "\\u0156", "343": "\\u0157", "8969": "\\u2309", "125": "\\u007D", "1056": "\\u0420", "1088": "\\u0440", "10551": "\\u2937", "10601": "\\u2969", "8221": "\\u201D", "8221": "\\u201D", "8627": "\\u21B3", "8476": "\\u211C", "8475": "\\u211B", "8476": "\\u211C", "8477": "\\u211D", "8476": "\\u211C", "9645": "\\u25AD", "174": "\\u00AE", "174": "\\u00AE", "174": "\\u00AE", "174": "\\u00AE", "8715": "\\u220B", "8651": "\\u21CB", "10607": "\\u296F", "10621": "\\u297D", "8971": "\\u230B", "120111": "\\uD835\\uDD2F", "8476": "\\u211C", "10596": "\\u2964", "8641": "\\u21C1", "8640": "\\u21C0", "10604": "\\u296C", "929": "\\u03A1", "961": "\\u03C1", "1009": "\\u03F1", "10217": "\\u27E9", "8677": "\\u21E5", "8594": "\\u2192", "8594": "\\u2192", "8658": "\\u21D2", "8644": "\\u21C4", "8611": "\\u21A3", "8969": "\\u2309", "10215": "\\u27E7", "10589": "\\u295D", "10581": "\\u2955", "8642": "\\u21C2", "8971": "\\u230B", "8641": "\\u21C1", "8640": "\\u21C0", "8644": "\\u21C4", "8652": "\\u21CC", "8649": "\\u21C9", "8605": "\\u219D", "8614": "\\u21A6", "8866": "\\u22A2", "10587": "\\u295B", "8908": "\\u22CC", "10704": "\\u29D0", "8883": "\\u22B3", "8885": "\\u22B5", "10575": "\\u294F", "10588": "\\u295C", "10580": "\\u2954", "8638": "\\u21BE", "10579": "\\u2953", "8640": "\\u21C0", "730": "\\u02DA", "8787": "\\u2253", "8644": "\\u21C4", "8652": "\\u21CC", "8207": "\\u200F", "9137": "\\u23B1", "9137": "\\u23B1", "10990": "\\u2AEE", "10221": "\\u27ED", "8702": "\\u21FE", "10215": "\\u27E7", "10630": "\\u2986", "120163": "\\uD835\\uDD63", "8477": "\\u211D", "10798": "\\u2A2E", "10805": "\\u2A35", "10608": "\\u2970", "41": "\\u0029", "10644": "\\u2994", "10770": "\\u2A12", "8649": "\\u21C9", "8667": "\\u21DB", "8250": "\\u203A", "120007": "\\uD835\\uDCC7", "8475": "\\u211B", "8625": "\\u21B1", "8625": "\\u21B1", "93": "\\u005D", "8217": "\\u2019", "8217": "\\u2019", "8908": "\\u22CC", "8906": "\\u22CA", "9657": "\\u25B9", "8885": "\\u22B5", "9656": "\\u25B8", "10702": "\\u29CE", "10740": "\\u29F4", "10600": "\\u2968", "8478": "\\u211E", "346": "\\u015A", "347": "\\u015B", "8218": "\\u201A", "10936": "\\u2AB8", "352": "\\u0160", "353": "\\u0161", "10940": "\\u2ABC", "8827": "\\u227B", "8829": "\\u227D", "10928": "\\u2AB0", "10932": "\\u2AB4", "350": "\\u015E", "351": "\\u015F", "348": "\\u015C", "349": "\\u015D", "10938": "\\u2ABA", "10934": "\\u2AB6", "8937": "\\u22E9", "10771": "\\u2A13", "8831": "\\u227F", "1057": "\\u0421", "1089": "\\u0441", "8865": "\\u22A1", "8901": "\\u22C5", "10854": "\\u2A66", "10533": "\\u2925", "8600": "\\u2198", "8664": "\\u21D8", "8600": "\\u2198", "167": "\\u00A7", "167": "\\u00A7", "59": "\\u003B", "10537": "\\u2929", "8726": "\\u2216", "8726": "\\u2216", "10038": "\\u2736", "120086": "\\uD835\\uDD16", "120112": "\\uD835\\uDD30", "8994": "\\u2322", "9839": "\\u266F", "1065": "\\u0429", "1097": "\\u0449", "1064": "\\u0428", "1096": "\\u0448", "8595": "\\u2193", "8592": "\\u2190", "8739": "\\u2223", "8741": "\\u2225", "8594": "\\u2192", "8593": "\\u2191", "173": "\\u00AD", "173": "\\u00AD", "931": "\\u03A3", "963": "\\u03C3", "962": "\\u03C2", "962": "\\u03C2", "8764": "\\u223C", "10858": "\\u2A6A", "8771": "\\u2243", "8771": "\\u2243", "10910": "\\u2A9E", "10912": "\\u2AA0", "10909": "\\u2A9D", "10911": "\\u2A9F", "8774": "\\u2246", "10788": "\\u2A24", "10610": "\\u2972", "8592": "\\u2190", "8728": "\\u2218", "8726": "\\u2216", "10803": "\\u2A33", "10724": "\\u29E4", "8739": "\\u2223", "8995": "\\u2323", "10922": "\\u2AAA", "10924": "\\u2AAC", "10924": "\\u2AAC", "65024": "\\uFE00", "1068": "\\u042C", "1100": "\\u044C", "9023": "\\u233F", "10692": "\\u29C4", "47": "\\u002F", "120138": "\\uD835\\uDD4A", "120164": "\\uD835\\uDD64", "9824": "\\u2660", "9824": "\\u2660", "8741": "\\u2225", "8851": "\\u2293", "8851": "\\u2293", "65024": "\\uFE00", "8852": "\\u2294", "8852": "\\u2294", "65024": "\\uFE00", "8730": "\\u221A", "8847": "\\u228F", "8849": "\\u2291", "8847": "\\u228F", "8849": "\\u2291", "8848": "\\u2290", "8850": "\\u2292", "8848": "\\u2290", "8850": "\\u2292", "9633": "\\u25A1", "9633": "\\u25A1", "8851": "\\u2293", "8847": "\\u228F", "8849": "\\u2291", "8848": "\\u2290", "8850": "\\u2292", "8852": "\\u2294", "9642": "\\u25AA", "9633": "\\u25A1", "9642": "\\u25AA", "8594": "\\u2192", "119982": "\\uD835\\uDCAE", "120008": "\\uD835\\uDCC8", "8726": "\\u2216", "8995": "\\u2323", "8902": "\\u22C6", "8902": "\\u22C6", "9734": "\\u2606", "9733": "\\u2605", "1013": "\\u03F5", "981": "\\u03D5", "175": "\\u00AF", "8834": "\\u2282", "8912": "\\u22D0", "10941": "\\u2ABD", "10949": "\\u2AC5", "8838": "\\u2286", "10947": "\\u2AC3", "10945": "\\u2AC1", "10955": "\\u2ACB", "8842": "\\u228A", "10943": "\\u2ABF", "10617": "\\u2979", "8834": "\\u2282", "8912": "\\u22D0", "8838": "\\u2286", "10949": "\\u2AC5", "8838": "\\u2286", "8842": "\\u228A", "10955": "\\u2ACB", "10951": "\\u2AC7", "10965": "\\u2AD5", "10963": "\\u2AD3", "10936": "\\u2AB8", "8827": "\\u227B", "8829": "\\u227D", "8827": "\\u227B", "10928": "\\u2AB0", "8829": "\\u227D", "8831": "\\u227F", "10928": "\\u2AB0", "10938": "\\u2ABA", "10934": "\\u2AB6", "8937": "\\u22E9", "8831": "\\u227F", "8715": "\\u220B", "8721": "\\u2211", "8721": "\\u2211", "9834": "\\u266A", "185": "\\u00B9", "185": "\\u00B9", "178": "\\u00B2", "178": "\\u00B2", "179": "\\u00B3", "179": "\\u00B3", "8835": "\\u2283", "8913": "\\u22D1", "10942": "\\u2ABE", "10968": "\\u2AD8", "10950": "\\u2AC6", "8839": "\\u2287", "10948": "\\u2AC4", "8835": "\\u2283", "8839": "\\u2287", "10185": "\\u27C9", "10967": "\\u2AD7", "10619": "\\u297B", "10946": "\\u2AC2", "10956": "\\u2ACC", "8843": "\\u228B", "10944": "\\u2AC0", "8835": "\\u2283", "8913": "\\u22D1", "8839": "\\u2287", "10950": "\\u2AC6", "8843": "\\u228B", "10956": "\\u2ACC", "10952": "\\u2AC8", "10964": "\\u2AD4", "10966": "\\u2AD6", "10534": "\\u2926", "8601": "\\u2199", "8665": "\\u21D9", "8601": "\\u2199", "10538": "\\u292A", "223": "\\u00DF", "223": "\\u00DF", "9": "\\u0009", "8982": "\\u2316", "932": "\\u03A4", "964": "\\u03C4", "9140": "\\u23B4", "356": "\\u0164", "357": "\\u0165", "354": "\\u0162", "355": "\\u0163", "1058": "\\u0422", "1090": "\\u0442", "8411": "\\u20DB", "8981": "\\u2315", "120087": "\\uD835\\uDD17", "120113": "\\uD835\\uDD31", "8756": "\\u2234", "8756": "\\u2234", "8756": "\\u2234", "920": "\\u0398", "952": "\\u03B8", "977": "\\u03D1", "977": "\\u03D1", "8776": "\\u2248", "8764": "\\u223C", "8287": "\\u205F", "8202": "\\u200A", "8201": "\\u2009", "8201": "\\u2009", "8776": "\\u2248", "8764": "\\u223C", "222": "\\u00DE", "222": "\\u00DE", "254": "\\u00FE", "254": "\\u00FE", "732": "\\u02DC", "8764": "\\u223C", "8771": "\\u2243", "8773": "\\u2245", "8776": "\\u2248", "10801": "\\u2A31", "8864": "\\u22A0", "215": "\\u00D7", "215": "\\u00D7", "10800": "\\u2A30", "8749": "\\u222D", "10536": "\\u2928", "9014": "\\u2336", "10993": "\\u2AF1", "8868": "\\u22A4", "120139": "\\uD835\\uDD4B", "120165": "\\uD835\\uDD65", "10970": "\\u2ADA", "10537": "\\u2929", "8244": "\\u2034", "8482": "\\u2122", "8482": "\\u2122", "9653": "\\u25B5", "9663": "\\u25BF", "9667": "\\u25C3", "8884": "\\u22B4", "8796": "\\u225C", "9657": "\\u25B9", "8885": "\\u22B5", "9708": "\\u25EC", "8796": "\\u225C", "10810": "\\u2A3A", "8411": "\\u20DB", "10809": "\\u2A39", "10701": "\\u29CD", "10811": "\\u2A3B", "9186": "\\u23E2", "119983": "\\uD835\\uDCAF", "120009": "\\uD835\\uDCC9", "1062": "\\u0426", "1094": "\\u0446", "1035": "\\u040B", "1115": "\\u045B", "358": "\\u0166", "359": "\\u0167", "8812": "\\u226C", "8606": "\\u219E", "8608": "\\u21A0", "218": "\\u00DA", "218": "\\u00DA", "250": "\\u00FA", "250": "\\u00FA", "8593": "\\u2191", "8607": "\\u219F", "8657": "\\u21D1", "10569": "\\u2949", "1038": "\\u040E", "1118": "\\u045E", "364": "\\u016C", "365": "\\u016D", "219": "\\u00DB", "219": "\\u00DB", "251": "\\u00FB", "251": "\\u00FB", "1059": "\\u0423", "1091": "\\u0443", "8645": "\\u21C5", "368": "\\u0170", "369": "\\u0171", "10606": "\\u296E", "10622": "\\u297E", "120088": "\\uD835\\uDD18", "120114": "\\uD835\\uDD32", "217": "\\u00D9", "217": "\\u00D9", "249": "\\u00F9", "249": "\\u00F9", "10595": "\\u2963", "8639": "\\u21BF", "8638": "\\u21BE", "9600": "\\u2580", "8988": "\\u231C", "8988": "\\u231C", "8975": "\\u230F", "9720": "\\u25F8", "362": "\\u016A", "363": "\\u016B", "168": "\\u00A8", "168": "\\u00A8", "95": "\\u005F", "9183": "\\u23DF", "9141": "\\u23B5", "9181": "\\u23DD", "8899": "\\u22C3", "8846": "\\u228E", "370": "\\u0172", "371": "\\u0173", "120140": "\\uD835\\uDD4C", "120166": "\\uD835\\uDD66", "10514": "\\u2912", "8593": "\\u2191", "8593": "\\u2191", "8657": "\\u21D1", "8645": "\\u21C5", "8597": "\\u2195", "8597": "\\u2195", "8661": "\\u21D5", "10606": "\\u296E", "8639": "\\u21BF", "8638": "\\u21BE", "8846": "\\u228E", "8598": "\\u2196", "8599": "\\u2197", "965": "\\u03C5", "978": "\\u03D2", "978": "\\u03D2", "933": "\\u03A5", "965": "\\u03C5", "8613": "\\u21A5", "8869": "\\u22A5", "8648": "\\u21C8", "8989": "\\u231D", "8989": "\\u231D", "8974": "\\u230E", "366": "\\u016E", "367": "\\u016F", "9721": "\\u25F9", "119984": "\\uD835\\uDCB0", "120010": "\\uD835\\uDCCA", "8944": "\\u22F0", "360": "\\u0168", "361": "\\u0169", "9653": "\\u25B5", "9652": "\\u25B4", "8648": "\\u21C8", "220": "\\u00DC", "220": "\\u00DC", "252": "\\u00FC", "252": "\\u00FC", "10663": "\\u29A7", "10652": "\\u299C", "1013": "\\u03F5", "1008": "\\u03F0", "8709": "\\u2205", "981": "\\u03D5", "982": "\\u03D6", "8733": "\\u221D", "8597": "\\u2195", "8661": "\\u21D5", "1009": "\\u03F1", "962": "\\u03C2", "8842": "\\u228A", "65024": "\\uFE00", "10955": "\\u2ACB", "65024": "\\uFE00", "8843": "\\u228B", "65024": "\\uFE00", "10956": "\\u2ACC", "65024": "\\uFE00", "977": "\\u03D1", "8882": "\\u22B2", "8883": "\\u22B3", "10984": "\\u2AE8", "10987": "\\u2AEB", "10985": "\\u2AE9", "1042": "\\u0412", "1074": "\\u0432", "8866": "\\u22A2", "8872": "\\u22A8", "8873": "\\u22A9", "8875": "\\u22AB", "10982": "\\u2AE6", "8891": "\\u22BB", "8744": "\\u2228", "8897": "\\u22C1", "8794": "\\u225A", "8942": "\\u22EE", "124": "\\u007C", "8214": "\\u2016", "124": "\\u007C", "8214": "\\u2016", "8739": "\\u2223", "124": "\\u007C", "10072": "\\u2758", "8768": "\\u2240", "8202": "\\u200A", "120089": "\\uD835\\uDD19", "120115": "\\uD835\\uDD33", "8882": "\\u22B2", "8834": "\\u2282", "8402": "\\u20D2", "8835": "\\u2283", "8402": "\\u20D2", "120141": "\\uD835\\uDD4D", "120167": "\\uD835\\uDD67", "8733": "\\u221D", "8883": "\\u22B3", "119985": "\\uD835\\uDCB1", "120011": "\\uD835\\uDCCB", "10955": "\\u2ACB", "65024": "\\uFE00", "8842": "\\u228A", "65024": "\\uFE00", "10956": "\\u2ACC", "65024": "\\uFE00", "8843": "\\u228B", "65024": "\\uFE00", "8874": "\\u22AA", "10650": "\\u299A", "372": "\\u0174", "373": "\\u0175", "10847": "\\u2A5F", "8743": "\\u2227", "8896": "\\u22C0", "8793": "\\u2259", "8472": "\\u2118", "120090": "\\uD835\\uDD1A", "120116": "\\uD835\\uDD34", "120142": "\\uD835\\uDD4E", "120168": "\\uD835\\uDD68", "8472": "\\u2118", "8768": "\\u2240", "8768": "\\u2240", "119986": "\\uD835\\uDCB2", "120012": "\\uD835\\uDCCC", "8898": "\\u22C2", "9711": "\\u25EF", "8899": "\\u22C3", "9661": "\\u25BD", "120091": "\\uD835\\uDD1B", "120117": "\\uD835\\uDD35", "10231": "\\u27F7", "10234": "\\u27FA", "926": "\\u039E", "958": "\\u03BE", "10229": "\\u27F5", "10232": "\\u27F8", "10236": "\\u27FC", "8955": "\\u22FB", "10752": "\\u2A00", "120143": "\\uD835\\uDD4F", "120169": "\\uD835\\uDD69", "10753": "\\u2A01", "10754": "\\u2A02", "10230": "\\u27F6", "10233": "\\u27F9", "119987": "\\uD835\\uDCB3", "120013": "\\uD835\\uDCCD", "10758": "\\u2A06", "10756": "\\u2A04", "9651": "\\u25B3", "8897": "\\u22C1", "8896": "\\u22C0", "221": "\\u00DD", "221": "\\u00DD", "253": "\\u00FD", "253": "\\u00FD", "1071": "\\u042F", "1103": "\\u044F", "374": "\\u0176", "375": "\\u0177", "1067": "\\u042B", "1099": "\\u044B", "165": "\\u00A5", "165": "\\u00A5", "120092": "\\uD835\\uDD1C", "120118": "\\uD835\\uDD36", "1031": "\\u0407", "1111": "\\u0457", "120144": "\\uD835\\uDD50", "120170": "\\uD835\\uDD6A", "119988": "\\uD835\\uDCB4", "120014": "\\uD835\\uDCCE", "1070": "\\u042E", "1102": "\\u044E", "255": "\\u00FF", "255": "\\u00FF", "376": "\\u0178", "377": "\\u0179", "378": "\\u017A", "381": "\\u017D", "382": "\\u017E", "1047": "\\u0417", "1079": "\\u0437", "379": "\\u017B", "380": "\\u017C", "8488": "\\u2128", "8203": "\\u200B", "918": "\\u0396", "950": "\\u03B6", "120119": "\\uD835\\uDD37", "8488": "\\u2128", "1046": "\\u0416", "1078": "\\u0436", "8669": "\\u21DD", "120171": "\\uD835\\uDD6B", "8484": "\\u2124", "119989": "\\uD835\\uDCB5", "120015": "\\uD835\\uDCCF", "8205": "\\u200D", "8204": "\\u200C"}`);

// Diff algorithm
/**
 * This library modifies the diff-patch-match library by Neil Fraser
 * by removing the patch and match functionality and certain advanced
 * options in the diff function. The original license is as follows:
 *
 * ===
 *
 * Diff Match and Patch
 *
 * Copyright 2006 Google Inc.
 * http://code.google.com/p/google-diff-match-patch/
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


/**
 * The data structure representing a diff is an array of tuples:
 * [[DIFF_DELETE, 'Hello'], [DIFF_INSERT, 'Goodbye'], [DIFF_EQUAL, ' world.']]
 * which means: delete 'Hello', add 'Goodbye' and keep ' world.'
 */
var DIFF_DELETE = -1;
var DIFF_INSERT = 1;
var DIFF_EQUAL = 0;


/**
 * Find the differences between two texts.  Simplifies the problem by stripping
 * any common prefix or suffix off the texts before diffing.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @param {Int|Object} [cursor_pos] Edit position in text1 or object with more info
 * @return {Array} Array of diff tuples.
 */
function diff_main(text1, text2, cursor_pos, _fix_unicode) {
  // Check for equality
  if (text1 === text2) {
    if (text1) {
      return [[DIFF_EQUAL, text1]];
    }
    return [];
  }

  if (cursor_pos != null) {
    var editdiff = find_cursor_edit_diff(text1, text2, cursor_pos);
    if (editdiff) {
      return editdiff;
    }
  }

  // Trim off common prefix (speedup).
  var commonlength = diff_commonPrefix(text1, text2);
  var commonprefix = text1.substring(0, commonlength);
  text1 = text1.substring(commonlength);
  text2 = text2.substring(commonlength);

  // Trim off common suffix (speedup).
  commonlength = diff_commonSuffix(text1, text2);
  var commonsuffix = text1.substring(text1.length - commonlength);
  text1 = text1.substring(0, text1.length - commonlength);
  text2 = text2.substring(0, text2.length - commonlength);

  // Compute the diff on the middle block.
  var diffs = diff_compute_(text1, text2);

  // Restore the prefix and suffix.
  if (commonprefix) {
    diffs.unshift([DIFF_EQUAL, commonprefix]);
  }
  if (commonsuffix) {
    diffs.push([DIFF_EQUAL, commonsuffix]);
  }
  diff_cleanupMerge(diffs, _fix_unicode);
  return diffs;
};


/**
 * Find the differences between two texts.  Assumes that the texts do not
 * have any common prefix or suffix.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @return {Array} Array of diff tuples.
 */
function diff_compute_(text1, text2) {
  var diffs;

  if (!text1) {
    // Just add some text (speedup).
    return [[DIFF_INSERT, text2]];
  }

  if (!text2) {
    // Just delete some text (speedup).
    return [[DIFF_DELETE, text1]];
  }

  var longtext = text1.length > text2.length ? text1 : text2;
  var shorttext = text1.length > text2.length ? text2 : text1;
  var i = longtext.indexOf(shorttext);
  if (i !== -1) {
    // Shorter text is inside the longer text (speedup).
    diffs = [
      [DIFF_INSERT, longtext.substring(0, i)],
      [DIFF_EQUAL, shorttext],
      [DIFF_INSERT, longtext.substring(i + shorttext.length)]
    ];
    // Swap insertions for deletions if diff is reversed.
    if (text1.length > text2.length) {
      diffs[0][0] = diffs[2][0] = DIFF_DELETE;
    }
    return diffs;
  }

  if (shorttext.length === 1) {
    // Single character string.
    // After the previous speedup, the character can't be an equality.
    return [[DIFF_DELETE, text1], [DIFF_INSERT, text2]];
  }

  // Check to see if the problem can be split in two.
  var hm = diff_halfMatch_(text1, text2);
  if (hm) {
    // A half-match was found, sort out the return data.
    var text1_a = hm[0];
    var text1_b = hm[1];
    var text2_a = hm[2];
    var text2_b = hm[3];
    var mid_common = hm[4];
    // Send both pairs off for separate processing.
    var diffs_a = diff_main(text1_a, text2_a);
    var diffs_b = diff_main(text1_b, text2_b);
    // Merge the results.
    return diffs_a.concat([[DIFF_EQUAL, mid_common]], diffs_b);
  }

  return diff_bisect_(text1, text2);
};


/**
 * Find the 'middle snake' of a diff, split the problem in two
 * and return the recursively constructed diff.
 * See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @return {Array} Array of diff tuples.
 * @private
 */
function diff_bisect_(text1, text2) {
  // Cache the text lengths to prevent multiple calls.
  var text1_length = text1.length;
  var text2_length = text2.length;
  var max_d = Math.ceil((text1_length + text2_length) / 2);
  var v_offset = max_d;
  var v_length = 2 * max_d;
  var v1 = new Array(v_length);
  var v2 = new Array(v_length);
  // Setting all elements to -1 is faster in Chrome & Firefox than mixing
  // integers and undefined.
  for (var x = 0; x < v_length; x++) {
    v1[x] = -1;
    v2[x] = -1;
  }
  v1[v_offset + 1] = 0;
  v2[v_offset + 1] = 0;
  var delta = text1_length - text2_length;
  // If the total number of characters is odd, then the front path will collide
  // with the reverse path.
  var front = (delta % 2 !== 0);
  // Offsets for start and end of k loop.
  // Prevents mapping of space beyond the grid.
  var k1start = 0;
  var k1end = 0;
  var k2start = 0;
  var k2end = 0;
  for (var d = 0; d < max_d; d++) {
    // Walk the front path one step.
    for (var k1 = -d + k1start; k1 <= d - k1end; k1 += 2) {
      var k1_offset = v_offset + k1;
      var x1;
      if (k1 === -d || (k1 !== d && v1[k1_offset - 1] < v1[k1_offset + 1])) {
        x1 = v1[k1_offset + 1];
      } else {
        x1 = v1[k1_offset - 1] + 1;
      }
      var y1 = x1 - k1;
      while (
        x1 < text1_length && y1 < text2_length &&
        text1.charAt(x1) === text2.charAt(y1)
      ) {
        x1++;
        y1++;
      }
      v1[k1_offset] = x1;
      if (x1 > text1_length) {
        // Ran off the right of the graph.
        k1end += 2;
      } else if (y1 > text2_length) {
        // Ran off the bottom of the graph.
        k1start += 2;
      } else if (front) {
        var k2_offset = v_offset + delta - k1;
        if (k2_offset >= 0 && k2_offset < v_length && v2[k2_offset] !== -1) {
          // Mirror x2 onto top-left coordinate system.
          var x2 = text1_length - v2[k2_offset];
          if (x1 >= x2) {
            // Overlap detected.
            return diff_bisectSplit_(text1, text2, x1, y1);
          }
        }
      }
    }

    // Walk the reverse path one step.
    for (var k2 = -d + k2start; k2 <= d - k2end; k2 += 2) {
      var k2_offset = v_offset + k2;
      var x2;
      if (k2 === -d || (k2 !== d && v2[k2_offset - 1] < v2[k2_offset + 1])) {
        x2 = v2[k2_offset + 1];
      } else {
        x2 = v2[k2_offset - 1] + 1;
      }
      var y2 = x2 - k2;
      while (
        x2 < text1_length && y2 < text2_length &&
        text1.charAt(text1_length - x2 - 1) === text2.charAt(text2_length - y2 - 1)
      ) {
        x2++;
        y2++;
      }
      v2[k2_offset] = x2;
      if (x2 > text1_length) {
        // Ran off the left of the graph.
        k2end += 2;
      } else if (y2 > text2_length) {
        // Ran off the top of the graph.
        k2start += 2;
      } else if (!front) {
        var k1_offset = v_offset + delta - k2;
        if (k1_offset >= 0 && k1_offset < v_length && v1[k1_offset] !== -1) {
          var x1 = v1[k1_offset];
          var y1 = v_offset + x1 - k1_offset;
          // Mirror x2 onto top-left coordinate system.
          x2 = text1_length - x2;
          if (x1 >= x2) {
            // Overlap detected.
            return diff_bisectSplit_(text1, text2, x1, y1);
          }
        }
      }
    }
  }
  // Diff took too long and hit the deadline or
  // number of diffs equals number of characters, no commonality at all.
  return [[DIFF_DELETE, text1], [DIFF_INSERT, text2]];
};


/**
 * Given the location of the 'middle snake', split the diff in two parts
 * and recurse.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @param {number} x Index of split point in text1.
 * @param {number} y Index of split point in text2.
 * @return {Array} Array of diff tuples.
 */
function diff_bisectSplit_(text1, text2, x, y) {
  var text1a = text1.substring(0, x);
  var text2a = text2.substring(0, y);
  var text1b = text1.substring(x);
  var text2b = text2.substring(y);

  // Compute both diffs serially.
  var diffs = diff_main(text1a, text2a);
  var diffsb = diff_main(text1b, text2b);

  return diffs.concat(diffsb);
};


/**
 * Determine the common prefix of two strings.
 * @param {string} text1 First string.
 * @param {string} text2 Second string.
 * @return {number} The number of characters common to the start of each
 *     string.
 */
function diff_commonPrefix(text1, text2) {
  // Quick check for common null cases.
  if (!text1 || !text2 || text1.charAt(0) !== text2.charAt(0)) {
    return 0;
  }
  // Binary search.
  // Performance analysis: http://neil.fraser.name/news/2007/10/09/
  var pointermin = 0;
  var pointermax = Math.min(text1.length, text2.length);
  var pointermid = pointermax;
  var pointerstart = 0;
  while (pointermin < pointermid) {
    if (
      text1.substring(pointerstart, pointermid) ==
      text2.substring(pointerstart, pointermid)
    ) {
      pointermin = pointermid;
      pointerstart = pointermin;
    } else {
      pointermax = pointermid;
    }
    pointermid = Math.floor((pointermax - pointermin) / 2 + pointermin);
  }

  if (is_surrogate_pair_start(text1.charCodeAt(pointermid - 1))) {
    pointermid--;
  }

  return pointermid;
};


/**
 * Determine the common suffix of two strings.
 * @param {string} text1 First string.
 * @param {string} text2 Second string.
 * @return {number} The number of characters common to the end of each string.
 */
function diff_commonSuffix(text1, text2) {
  // Quick check for common null cases.
  if (!text1 || !text2 || text1.slice(-1) !== text2.slice(-1)) {
    return 0;
  }
  // Binary search.
  // Performance analysis: http://neil.fraser.name/news/2007/10/09/
  var pointermin = 0;
  var pointermax = Math.min(text1.length, text2.length);
  var pointermid = pointermax;
  var pointerend = 0;
  while (pointermin < pointermid) {
    if (
      text1.substring(text1.length - pointermid, text1.length - pointerend) ==
      text2.substring(text2.length - pointermid, text2.length - pointerend)
    ) {
      pointermin = pointermid;
      pointerend = pointermin;
    } else {
      pointermax = pointermid;
    }
    pointermid = Math.floor((pointermax - pointermin) / 2 + pointermin);
  }

  if (is_surrogate_pair_end(text1.charCodeAt(text1.length - pointermid))) {
    pointermid--;
  }

  return pointermid;
};


/**
 * Do the two texts share a substring which is at least half the length of the
 * longer text?
 * This speedup can produce non-minimal diffs.
 * @param {string} text1 First string.
 * @param {string} text2 Second string.
 * @return {Array.<string>} Five element Array, containing the prefix of
 *     text1, the suffix of text1, the prefix of text2, the suffix of
 *     text2 and the common middle.  Or null if there was no match.
 */
function diff_halfMatch_(text1, text2) {
  var longtext = text1.length > text2.length ? text1 : text2;
  var shorttext = text1.length > text2.length ? text2 : text1;
  if (longtext.length < 4 || shorttext.length * 2 < longtext.length) {
    return null;  // Pointless.
  }

  /**
   * Does a substring of shorttext exist within longtext such that the substring
   * is at least half the length of longtext?
   * Closure, but does not reference any external variables.
   * @param {string} longtext Longer string.
   * @param {string} shorttext Shorter string.
   * @param {number} i Start index of quarter length substring within longtext.
   * @return {Array.<string>} Five element Array, containing the prefix of
   *     longtext, the suffix of longtext, the prefix of shorttext, the suffix
   *     of shorttext and the common middle.  Or null if there was no match.
   * @private
   */
  function diff_halfMatchI_(longtext, shorttext, i) {
    // Start with a 1/4 length substring at position i as a seed.
    var seed = longtext.substring(i, i + Math.floor(longtext.length / 4));
    var j = -1;
    var best_common = '';
    var best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b;
    while ((j = shorttext.indexOf(seed, j + 1)) !== -1) {
      var prefixLength = diff_commonPrefix(
        longtext.substring(i), shorttext.substring(j));
      var suffixLength = diff_commonSuffix(
        longtext.substring(0, i), shorttext.substring(0, j));
      if (best_common.length < suffixLength + prefixLength) {
        best_common = shorttext.substring(
          j - suffixLength, j) + shorttext.substring(j, j + prefixLength);
        best_longtext_a = longtext.substring(0, i - suffixLength);
        best_longtext_b = longtext.substring(i + prefixLength);
        best_shorttext_a = shorttext.substring(0, j - suffixLength);
        best_shorttext_b = shorttext.substring(j + prefixLength);
      }
    }
    if (best_common.length * 2 >= longtext.length) {
      return [
        best_longtext_a, best_longtext_b,
        best_shorttext_a, best_shorttext_b, best_common
      ];
    } else {
      return null;
    }
  }

  // First check if the second quarter is the seed for a half-match.
  var hm1 = diff_halfMatchI_(longtext, shorttext, Math.ceil(longtext.length / 4));
  // Check again based on the third quarter.
  var hm2 = diff_halfMatchI_(longtext, shorttext, Math.ceil(longtext.length / 2));
  var hm;
  if (!hm1 && !hm2) {
    return null;
  } else if (!hm2) {
    hm = hm1;
  } else if (!hm1) {
    hm = hm2;
  } else {
    // Both matched.  Select the longest.
    hm = hm1[4].length > hm2[4].length ? hm1 : hm2;
  }

  // A half-match was found, sort out the return data.
  var text1_a, text1_b, text2_a, text2_b;
  if (text1.length > text2.length) {
    text1_a = hm[0];
    text1_b = hm[1];
    text2_a = hm[2];
    text2_b = hm[3];
  } else {
    text2_a = hm[0];
    text2_b = hm[1];
    text1_a = hm[2];
    text1_b = hm[3];
  }
  var mid_common = hm[4];
  return [text1_a, text1_b, text2_a, text2_b, mid_common];
};


/**
 * Reorder and merge like edit sections.  Merge equalities.
 * Any edit section can move as long as it doesn't cross an equality.
 * @param {Array} diffs Array of diff tuples.
 * @param {boolean} fix_unicode Whether to normalize to a unicode-correct diff
 */
function diff_cleanupMerge(diffs, fix_unicode) {
  diffs.push([DIFF_EQUAL, '']);  // Add a dummy entry at the end.
  var pointer = 0;
  var count_delete = 0;
  var count_insert = 0;
  var text_delete = '';
  var text_insert = '';
  var commonlength;
  while (pointer < diffs.length) {
    if (pointer < diffs.length - 1 && !diffs[pointer][1]) {
      diffs.splice(pointer, 1);
      continue;
    }
    switch (diffs[pointer][0]) {
      case DIFF_INSERT:

        count_insert++;
        text_insert += diffs[pointer][1];
        pointer++;
        break;
      case DIFF_DELETE:
        count_delete++;
        text_delete += diffs[pointer][1];
        pointer++;
        break;
      case DIFF_EQUAL:
        var previous_equality = pointer - count_insert - count_delete - 1;
        if (fix_unicode) {
          // prevent splitting of unicode surrogate pairs.  when fix_unicode is true,
          // we assume that the old and new text in the diff are complete and correct
          // unicode-encoded JS strings, but the tuple boundaries may fall between
          // surrogate pairs.  we fix this by shaving off stray surrogates from the end
          // of the previous equality and the beginning of this equality.  this may create
          // empty equalities or a common prefix or suffix.  for example, if AB and AC are
          // emojis, `[[0, 'A'], [-1, 'BA'], [0, 'C']]` would turn into deleting 'ABAC' and
          // inserting 'AC', and then the common suffix 'AC' will be eliminated.  in this
          // particular case, both equalities go away, we absorb any previous inequalities,
          // and we keep scanning for the next equality before rewriting the tuples.
          if (previous_equality >= 0 && ends_with_pair_start(diffs[previous_equality][1])) {
            var stray = diffs[previous_equality][1].slice(-1);
            diffs[previous_equality][1] = diffs[previous_equality][1].slice(0, -1);
            text_delete = stray + text_delete;
            text_insert = stray + text_insert;
            if (!diffs[previous_equality][1]) {
              // emptied out previous equality, so delete it and include previous delete/insert
              diffs.splice(previous_equality, 1);
              pointer--;
              var k = previous_equality - 1;
              if (diffs[k] && diffs[k][0] === DIFF_INSERT) {
                count_insert++;
                text_insert = diffs[k][1] + text_insert;
                k--;
              }
              if (diffs[k] && diffs[k][0] === DIFF_DELETE) {
                count_delete++;
                text_delete = diffs[k][1] + text_delete;
                k--;
              }
              previous_equality = k;
            }
          }
          if (starts_with_pair_end(diffs[pointer][1])) {
            var stray = diffs[pointer][1].charAt(0);
            diffs[pointer][1] = diffs[pointer][1].slice(1);
            text_delete += stray;
            text_insert += stray;
          }
        }
        if (pointer < diffs.length - 1 && !diffs[pointer][1]) {
          // for empty equality not at end, wait for next equality
          diffs.splice(pointer, 1);
          break;
        }
        if (text_delete.length > 0 || text_insert.length > 0) {
          // note that diff_commonPrefix and diff_commonSuffix are unicode-aware
          if (text_delete.length > 0 && text_insert.length > 0) {
            // Factor out any common prefixes.
            commonlength = diff_commonPrefix(text_insert, text_delete);
            if (commonlength !== 0) {
              if (previous_equality >= 0) {
                diffs[previous_equality][1] += text_insert.substring(0, commonlength);
              } else {
                diffs.splice(0, 0, [DIFF_EQUAL, text_insert.substring(0, commonlength)]);
                pointer++;
              }
              text_insert = text_insert.substring(commonlength);
              text_delete = text_delete.substring(commonlength);
            }
            // Factor out any common suffixes.
            commonlength = diff_commonSuffix(text_insert, text_delete);
            if (commonlength !== 0) {
              diffs[pointer][1] =
                text_insert.substring(text_insert.length - commonlength) + diffs[pointer][1];
              text_insert = text_insert.substring(0, text_insert.length - commonlength);
              text_delete = text_delete.substring(0, text_delete.length - commonlength);
            }
          }
          // Delete the offending records and add the merged ones.
          var n = count_insert + count_delete;
          if (text_delete.length === 0 && text_insert.length === 0) {
            diffs.splice(pointer - n, n);
            pointer = pointer - n;
          } else if (text_delete.length === 0) {
            diffs.splice(pointer - n, n, [DIFF_INSERT, text_insert]);
            pointer = pointer - n + 1;
          } else if (text_insert.length === 0) {
            diffs.splice(pointer - n, n, [DIFF_DELETE, text_delete]);
            pointer = pointer - n + 1;
          } else {
            diffs.splice(pointer - n, n, [DIFF_DELETE, text_delete], [DIFF_INSERT, text_insert]);
            pointer = pointer - n + 2;
          }
        }
        if (pointer !== 0 && diffs[pointer - 1][0] === DIFF_EQUAL) {
          // Merge this equality with the previous one.
          diffs[pointer - 1][1] += diffs[pointer][1];
          diffs.splice(pointer, 1);
        } else {
          pointer++;
        }
        count_insert = 0;
        count_delete = 0;
        text_delete = '';
        text_insert = '';
        break;
    }
  }
  if (diffs[diffs.length - 1][1] === '') {
    diffs.pop();  // Remove the dummy entry at the end.
  }

  // Second pass: look for single edits surrounded on both sides by equalities
  // which can be shifted sideways to eliminate an equality.
  // e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
  var changes = false;
  pointer = 1;
  // Intentionally ignore the first and last element (don't need checking).
  while (pointer < diffs.length - 1) {
    if (diffs[pointer - 1][0] === DIFF_EQUAL &&
      diffs[pointer + 1][0] === DIFF_EQUAL) {
      // This is a single edit surrounded by equalities.
      if (diffs[pointer][1].substring(diffs[pointer][1].length -
        diffs[pointer - 1][1].length) === diffs[pointer - 1][1]) {
        // Shift the edit over the previous equality.
        diffs[pointer][1] = diffs[pointer - 1][1] +
          diffs[pointer][1].substring(0, diffs[pointer][1].length -
            diffs[pointer - 1][1].length);
        diffs[pointer + 1][1] = diffs[pointer - 1][1] + diffs[pointer + 1][1];
        diffs.splice(pointer - 1, 1);
        changes = true;
      } else if (diffs[pointer][1].substring(0, diffs[pointer + 1][1].length) ==
        diffs[pointer + 1][1]) {
        // Shift the edit over the next equality.
        diffs[pointer - 1][1] += diffs[pointer + 1][1];
        diffs[pointer][1] =
          diffs[pointer][1].substring(diffs[pointer + 1][1].length) +
          diffs[pointer + 1][1];
        diffs.splice(pointer + 1, 1);
        changes = true;
      }
    }
    pointer++;
  }
  // If shifts were made, the diff needs reordering and another shift sweep.
  if (changes) {
    diff_cleanupMerge(diffs, fix_unicode);
  }
};

function is_surrogate_pair_start(charCode) {
  return charCode >= 0xD800 && charCode <= 0xDBFF;
}

function is_surrogate_pair_end(charCode) {
  return charCode >= 0xDC00 && charCode <= 0xDFFF;
}

function starts_with_pair_end(str) {
  return is_surrogate_pair_end(str.charCodeAt(0));
}

function ends_with_pair_start(str) {
  return is_surrogate_pair_start(str.charCodeAt(str.length - 1));
}

function remove_empty_tuples(tuples) {
  var ret = [];
  for (var i = 0; i < tuples.length; i++) {
    if (tuples[i][1].length > 0) {
      ret.push(tuples[i]);
    }
  }
  return ret;
}

function make_edit_splice(before, oldMiddle, newMiddle, after) {
  if (ends_with_pair_start(before) || starts_with_pair_end(after)) {
    return null;
  }
  return remove_empty_tuples([
    [DIFF_EQUAL, before],
    [DIFF_DELETE, oldMiddle],
    [DIFF_INSERT, newMiddle],
    [DIFF_EQUAL, after]
  ]);
}

function find_cursor_edit_diff(oldText, newText, cursor_pos) {
  // note: this runs after equality check has ruled out exact equality
  var oldRange = typeof cursor_pos === 'number' ?
    { index: cursor_pos, length: 0 } : cursor_pos.oldRange;
  var newRange = typeof cursor_pos === 'number' ?
    null : cursor_pos.newRange;
  // take into account the old and new selection to generate the best diff
  // possible for a text edit.  for example, a text change from "xxx" to "xx"
  // could be a delete or forwards-delete of any one of the x's, or the
  // result of selecting two of the x's and typing "x".
  var oldLength = oldText.length;
  var newLength = newText.length;
  if (oldRange.length === 0 && (newRange === null || newRange.length === 0)) {
    // see if we have an insert or delete before or after cursor
    var oldCursor = oldRange.index;
    var oldBefore = oldText.slice(0, oldCursor);
    var oldAfter = oldText.slice(oldCursor);
    var maybeNewCursor = newRange ? newRange.index : null;
    editBefore: {
      // is this an insert or delete right before oldCursor?
      var newCursor = oldCursor + newLength - oldLength;
      if (maybeNewCursor !== null && maybeNewCursor !== newCursor) {
        break editBefore;
      }
      if (newCursor < 0 || newCursor > newLength) {
        break editBefore;
      }
      var newBefore = newText.slice(0, newCursor);
      var newAfter = newText.slice(newCursor);
      if (newAfter !== oldAfter) {
        break editBefore;
      }
      var prefixLength = Math.min(oldCursor, newCursor);
      var oldPrefix = oldBefore.slice(0, prefixLength);
      var newPrefix = newBefore.slice(0, prefixLength);
      if (oldPrefix !== newPrefix) {
        break editBefore;
      }
      var oldMiddle = oldBefore.slice(prefixLength);
      var newMiddle = newBefore.slice(prefixLength);
      return make_edit_splice(oldPrefix, oldMiddle, newMiddle, oldAfter);
    }
    editAfter: {
      // is this an insert or delete right after oldCursor?
      if (maybeNewCursor !== null && maybeNewCursor !== oldCursor) {
        break editAfter;
      }
      var cursor = oldCursor;
      var newBefore = newText.slice(0, cursor);
      var newAfter = newText.slice(cursor);
      if (newBefore !== oldBefore) {
        break editAfter;
      }
      var suffixLength = Math.min(oldLength - cursor, newLength - cursor);
      var oldSuffix = oldAfter.slice(oldAfter.length - suffixLength);
      var newSuffix = newAfter.slice(newAfter.length - suffixLength);
      if (oldSuffix !== newSuffix) {
        break editAfter;
      }
      var oldMiddle = oldAfter.slice(0, oldAfter.length - suffixLength);
      var newMiddle = newAfter.slice(0, newAfter.length - suffixLength);
      return make_edit_splice(oldBefore, oldMiddle, newMiddle, oldSuffix);
    }
  }
  if (oldRange.length > 0 && newRange && newRange.length === 0) {
    replaceRange: {
      // see if diff could be a splice of the old selection range
      var oldPrefix = oldText.slice(0, oldRange.index);
      var oldSuffix = oldText.slice(oldRange.index + oldRange.length);
      var prefixLength = oldPrefix.length;
      var suffixLength = oldSuffix.length;
      if (newLength < prefixLength + suffixLength) {
        break replaceRange;
      }
      var newPrefix = newText.slice(0, prefixLength);
      var newSuffix = newText.slice(newLength - suffixLength);
      if (oldPrefix !== newPrefix || oldSuffix !== newSuffix) {
        break replaceRange;
      }
      var oldMiddle = oldText.slice(prefixLength, oldLength - suffixLength);
      var newMiddle = newText.slice(prefixLength, newLength - suffixLength);
      return make_edit_splice(oldPrefix, oldMiddle, newMiddle, oldSuffix);
    }
  }

  return null;
}

function diff(text1, text2, cursor_pos) {
  // only pass fix_unicode=true at the top level, not when diff_main is
  // recursively invoked
  return diff_main(text1, text2, cursor_pos, true);
}

diff.INSERT = DIFF_INSERT;
diff.DELETE = DIFF_DELETE;
diff.EQUAL = DIFF_EQUAL;

var _user$project$Native_ImpureGoodies = {
    randomFloat : function(_) {
      return Math.random();
    },

    crashToNothing : function(thunk) {
      try {
        // Just (thunk ())
        var result = thunk({ctor: '_Tuple0'});
        return _elm_lang$core$Maybe$Just(result);
      } catch(err) {
        if (err.ctor === undefined) { // Internal crash, not something thrown with ImpureGoodies.throw below.
          // Nothing
          console.log(err);
          return _elm_lang$core$Maybe$Nothing;
        } else {
          throw err;
        }
      }
    },

    stringCharAt : function(index) {
      return function(string) {
        if(index >= string.length || index < 0) {
          return _elm_lang$core$Maybe$Nothing;
        } else {
          return _elm_lang$core$Maybe$Just(string[index]);
        }
      }
    },

    crashToError : function(thunk) {
      try {
        // Ok (thunk ())
        var result = thunk({ctor: '_Tuple0'});
        return _elm_lang$core$Result$Ok(result);
      } catch(err) {
        if (err.ctor === undefined) { // Internal crash, not something thrown with ImpureGoodies.throw below.
          // Err (toString err)
          return _elm_lang$core$Result$Err(err.toString());
        } else {
          throw err;
        }
      }
    },

    throw : function(exception) {
      throw(exception)
    },

    tryCatch : function(exceptionConstructorName) { return function(thunk) { return function(catchThunk) {
      try {
        return thunk({ctor: '_Tuple0'});
      } catch(exception) {
        if (exception.ctor === exceptionConstructorName) {
          return catchThunk(exception);
        } else {
          throw exception;
        }
      }
    }}},

    mutateRecordField : function(record) { return function(fieldName) { return function(newValue) {
      // Sanity check.
      if (typeof record[fieldName] == typeof newValue) {
        record[fieldName] = newValue;
        return record;
      } else {
        throw "ImpureGoodies.mutateRecordField: types do not match" + (typeof record[fieldName]) + " vs " + (typeof newValue);
      }
    }}},

    putCache: function(record) { return function(cacheName) { return function(newValue) {
      if(typeof record == "object") {
        //record[" cache_" + cacheName] = newValue;
        console.log("stored cache " + cacheName, record)
      } else
        throw "ImpureGoodies.putCache: this is not an object";
      return newValue;
    }}},

    getCache: function(record) { return function(cacheName) {
      if(typeof record != "object")
        throw "ImpureGoodies.putCache: this is not an object";
      console.log("getting cache " + cacheName, record)
      var res = record[" cache_" + cacheName];
      if (typeof res == "undefined")
        return _elm_lang$core$Maybe$Nothing;
      else
        return _elm_lang$core$Maybe$Just(res);
    }},

    toggleGlobalBool : function(_) {
      __globalBoolState__ = !__globalBoolState__;
      return __globalBoolState__;
    },

    getCurrentTime : function() {
      return (new Date()).getTime();
    },

    timedRun : function(thunk) {
      var start = (new Date()).getTime();
      var result = thunk(_elm_lang$core$Native_Utils.Tuple0);
      var end = (new Date()).getTime();

      return _elm_lang$core$Native_Utils.Tuple2(result, end-start);
    },

    evaluate : function(string) {
      return eval(string);
    },

    log : function(string) {
      console.log(string);
      return string;
    },

    htmlescape: (function() {
      var tagsToReplace = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;'
      };

      function replaceTag(tag) {
        return tagsToReplace[tag] || tag;
      }
      function safe_tags_replace(str) {
        return str.replace(/[&<>]/g, replaceTag);
      }
      return function(string) {
        return safe_tags_replace(string);
      }
    })(),

    htmlunescape: (function() {
      var tagsToReplace = {
        '&amp;': '&',
         '&lt;': '<',
        '&gt;': '>'
      };

      function replaceTag(tag) {
        return tagsToReplace[tag] || tag;
      }
      function safe_tags_replace(str) {
        return str.replace(/&(amp|lt|gt);/g, replaceTag);
      }
      return function(string) {
        return safe_tags_replace(string);
      }
    })(),

    emptyNativeRecord: function(dummy) {
      return {};
    },

    addPairToNativeRecord: function(key) {
      return function(value) {
        return function(record) {
          record[key] = value;
          return record;
        }
      }
    },

    setValueToNativeRecord: function(key) {
      return function(mbValue) {
        return function(record) {
          if(typeof record  == "object") {
            if(mbValue.ctor == "Nothing") {
              delete record[key];
            } else {
              record[key] = mbValue._0;
            }
          }
          return record;
        }
      }
    },

    keyPairsOfNativeRecord: function(record) {
      var recordKeys = Object.keys(record);
      var acc = _elm_lang$core$Native_List.Nil;
      for(var i = 0; i < recordKeys.length; i ++) {
        var key = recordKeys[i];
        var value = record[key];
        acc = _elm_lang$core$Native_List.Cons(_elm_lang$core$Native_Utils.Tuple2(key, value), acc)
      }
      return acc;
    },

    nativeRecordGet: function(key) {
      return function(record) {
        if(typeof record == "object") {
          if(typeof record[key] == "undefined") {
              return _elm_lang$core$Maybe$Nothing;
          } else {
            return _elm_lang$core$Maybe$Just(record[key]);
          }
        } else return _elm_lang$core$Maybe$Nothing;
      }
    },

    nativeRecordKeys: function(record) {
     var result = Object.keys(record);
     var listResult = {ctor:"[]"};
     for(var i = result.length - 1; i >= 0; i--) {
       listResult = {ctor:"::",_0:result[i],_1:listResult};
     }
     return listResult;
    },

    toNativeArray: function(elmList) {
      var acc = [];
      while(elmList.ctor != "[]") {
        acc.push(elmList._0);
        elmList = elmList._1;
      }
      return acc;
    },

    fromNativeArray: function(v) {
      var result = _elm_lang$core$Native_List.Nil;
      for(var i = v.length - 1; i >= 0; i-- ) {
        result = _elm_lang$core$Native_List.Cons(v[i], result)
      }
      return result;
    },

    fromNative: (() => {
       var rec = v => stringCallback => numCallback => boolCallback => listCallback => listRecordCallback => functionCallback => {
          if(typeof v == "string") return stringCallback(v);
          if(typeof v == "number") return numCallback(v);
          if(typeof v == "boolean") return boolCallback(v);
          if(typeof v == "function") return functionCallback(v);
          if(typeof v == "object") {
            if(Array.isArray(v)) {
              var result = _elm_lang$core$Native_List.Nil;
              for(var i = v.length - 1; i >= 0; i-- ) {
                result = _elm_lang$core$Native_List.Cons(v[i], result)
              }
              return listCallback(result)
            } else {
              return listRecordCallback(_user$project$Native_ImpureGoodies.keyPairsOfNativeRecord(v))
            }
          }
        }
       return rec
      })(),

    hideType: function(v) {
      return v;
    },

    fromHtmlEntity: function(str) { // Accepts full named entities (e.g. &amp; or numbers like 20)
        if(typeof htmlNamedEntities[str] !== "undefined") {
          return _elm_lang$core$Maybe$Just(htmlNamedEntities[str]);
        } else {
          if(str.startsWith("x")) {
            str = str.substring(1);
            var n = parseInt(str, 16);
            str = "" + n;
          }
          if(typeof htmlNumEntities[str] !== "undefined") {
            return _elm_lang$core$Maybe$Just(htmlNumEntities[str]);
          } else { // If the string starts with x, it is a hexadecimal value
            return _elm_lang$core$Maybe$Nothing;
          }
        }
      },

    matchBaseValue: a => boolc => stringc => intc => {
      if(typeof a == "boolean")
        return {ctor: "Ok", _0: boolc(a)};
      if(typeof a == "string")
        return {ctor: "Ok", _0: stringc(a)};
      if(typeof a == "number")
        return {ctor: "Ok", _0: intc(a)};
      else
        return {ctor: "Err", _0: "" + a + " is not a boolean, string or number"};
    },
    whitespaceFromMetadata: metadata => {

      var commentCompatible = JSON.stringify(metadata).replace(/\\/g,"\\\\").replace(/\{-/g,"{\\-").replace(/-\}/g,"-\\}");
      return " {-" + commentCompatible + "-}";
    },
    whitespaceToMetadata: whitespace => {
      var extracted = /^\s*\{-(.*)-}$/g.exec(whitespace);
      if(extracted) {
        var metadata = JSON.parse(extracted[1].replace(/\{\\-/g, "{-").replace(/-\\\}/g, "-}").replace(/\\\\/g,"\\"))
        return metadata;
      } else return {}
    },
    diffString: a => b => {
       var v = diff(a, b);
       var result = _elm_lang$core$Native_List.Nil;
       for(var i = v.length - 1; i >= 0; i-- ) {
         var x = v[i];
         var elem = {ctor: x[0] === 0 ? "DiffEqual" :
                           x[0] === -1 ? "DiffRemoved" :
                                         "DiffAdded", _0: x[1]};
         result = _elm_lang$core$Native_List.Cons(elem, result)
       }
       return result;
    }
};
