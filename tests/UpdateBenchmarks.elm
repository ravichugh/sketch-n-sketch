module UpdateBenchmarks exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)
import UpdateRegex exposing (..)
import UpdateStack exposing (..)
import UpdateUtils exposing (..)
import Lang exposing (..)
import Regex exposing (replace, regex, HowMany(..))
import Utils
import Eval
import Syntax
import Lazy
import Results
import LazyList
import LangUtils exposing (..)
import ParserUtils
import HTMLValParser
import Set
import ExamplesGenerated
import Dict
import EvalUpdate
import ImpureGoodies
import LangSvg
import HTMLParser
import HTMLValParser
import Dict exposing (Dict)


exportMode = True {-- -- Add a } to make the benchmark run for all
                    && False --}
only1 = False {-- -- Add a } to make the benchmark run for all
  && False --}
bypassUnopt = False{-- -- Add a } to make the benchmark bypass the unopt
  || True --}
--displayCache = True

nToAverageOn = if only1 then 1 else 10

type alias CachedBenchmark = (List Float, List ((Float, List Float), (List Int, List Float)), List ((Float, List Float), (List Int, List Float)))

fromSuccessiveRuns: List CachedBenchmark -> CachedBenchmark
fromSuccessiveRuns l = let (l1, l2, l3) = Utils.unzip3 l in
  let ll1 = List.concatMap identity l1 in
  let ll2 = List.concatMap identity l2 in
  let ll3 = List.concatMap identity l3 in
  (ll1, ll2, ll3)

gather: List (String, CachedBenchmark) ->
        List (String, CachedBenchmark)
gather l =
  let aux l revAcc = case l of
    [] -> List.reverse revAcc
    (name, data)::tail ->
       let (dataForName, newTail) = tail |> List.partition (\(otherName, _) -> name == otherName) in
       let (_, otherData) = List.unzip dataForName in
       (name, fromSuccessiveRuns (data::otherData)) :: revAcc |>
       aux newTail
  in aux l []

next = "next"
(%%%) : a -> b -> a
(%%%) a b = a
benchmarkCache: Dict String CachedBenchmark
benchmarkCache = Dict.fromList <| gather <| [ ("", ([], [], []))
-- TODO: Below, paste the content of the file that we export to beneficiate from the cache.
-- START OF PASTE
   %%% {-
\newcommand{\benchmarks}{
%
% Benchmark Rows
%
% \tableRow {                    } {   } {    } {     Session     } {  Fastest Upd  } {  Slowest Upd  } {  Average Upd  } { #Amb     } {  Fastest Amb. } {  Slowest Amb. } {  Average Amb. }
%\tableRow   {Linked-Text         } { 91} {8547} {  0:53   } {  5  } {6658/1596\speedup{4} & 14536/2901\speedup{5} & 10855/2025\speedup{5}} {  \fromto{1}  {2}     } {   1.2   }
\tableRow   {Linked-Text         } { 91} {8547} {  0:53   } {  5  } {1886&\plusminus{140}&\speedup{3} & 2252&\plusminus{300}&\speedup{5} & 2025&\plusminus{200}&\speedup{5}} {  \fromto{1}  {2}     } {   1.2   }
 %%% -} next, ("Linked-Text",([884,902,914,820,777,815,833,832,845,925],[((11364,[2110,1937,2189,2348,2182]),([1,1,1,1,2],[0,0,0,0,1])),((11033,[2217,2153,1949,2135,1992]),([1,1,1,1,2],[1,0,0,0,2])),((9321,[1870,1812,1703,1802,1710]),([1,1,1,1,2],[0,0,0,0,1])),((9655,[1772,1888,1809,1787,1955]),([1,1,1,1,2],[0,0,0,0,2])),((10301,[1978,1901,1969,1990,1994]),([1,1,1,1,2],[0,0,0,0,1])),((11046,[1903,2141,1934,1940,2464]),([1,1,1,1,2],[0,0,0,0,2])),((11906,[2901,2164,2162,2057,1980]),([1,1,1,1,2],[0,0,0,0,1])),((11538,[2257,2359,2173,2130,2101]),([1,1,1,1,2],[0,0,0,0,2])),((11222,[2124,2455,2064,2039,2009]),([1,1,1,1,2],[0,0,0,0,1])),((9233,[1950,1895,1596,1618,1629]),([1,1,1,1,2],[0,0,0,0,1]))],[((51781,[11886,12366,12191,7508,7352]),([1,1,1,1,1],[0,0,0,0,0])),((51154,[11938,12097,11737,6658,8249]),([1,1,1,1,1],[0,0,0,0,0])),((55379,[13175,12868,13305,8098,7438]),([1,1,1,1,1],[0,0,0,0,0])),((58094,[12846,14204,14536,8619,7307]),([1,1,1,1,1],[0,0,0,0,0])),((58495,[13245,13793,13536,8459,8887]),([1,1,1,1,1],[0,0,0,0,0])),((56469,[14108,13544,13365,7141,7804]),([1,1,1,1,1],[0,0,0,0,0])),((54262,[12596,13497,13084,7210,7426]),([1,1,1,1,1],[0,0,0,0,0])),((55861,[13339,13077,13350,7900,7682]),([1,1,1,1,1],[0,0,0,0,0])),((53034,[12810,12918,12196,6991,7630]),([1,1,1,1,1],[0,0,0,0,0])),((53242,[12342,12620,12605,7994,7195]),([1,1,1,1,1],[0,0,0,0,0]))])) %%% {-
%\tableRow   {MVC                 } { 71} {7191} {  1:11   } {  10 } {9058/196\speedup{40} & 17796/801\speedup{20} & 12510/289\speedup{40}} {  \always{1} } {1        }
\tableRow   {MVC                 } { 71} {7191} {  1:11   } {  10 } {216&\plusminus{10}&\speedup{40} & 483&\plusminus{120}&\speedup{30} & 289&\plusminus{80}&\speedup{40}} {  \always{1} } {1        }
 %%% -} next, ("MVC",([713,690,687,745,662,648,732,693,805,816],[((3483,[359,290,264,462,225,269,337,258,202,350]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1,0])),((3061,[221,233,237,355,218,324,286,292,241,264]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((3059,[230,399,218,329,216,229,259,270,263,251]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,1,0,0,0,1,0,0])),((3060,[224,217,230,322,347,268,264,316,253,247]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((3264,[227,269,262,378,227,294,462,271,212,269]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1,0])),((3177,[239,239,254,373,216,253,513,276,218,234]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((3233,[204,227,210,345,196,251,529,393,231,285]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,1])),((3833,[243,292,241,381,214,305,294,801,319,236]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((3431,[224,361,561,338,218,265,258,284,261,275]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((3268,[268,245,270,393,348,247,305,288,241,271]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,1,0,0]))],[((118272,[12638,12681,9058,10411,12145,12197,12540,13075,13177,9976]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((122507,[12575,12338,9905,10301,13695,12223,13145,14629,13154,10167]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((124783,[13102,13527,12299,9993,12858,14714,12925,12196,12489,10301]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((135178,[13178,14675,10486,10042,14552,17796,16802,13312,12700,11215]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((122969,[13464,13261,10212,10139,13135,12733,13178,13527,13293,9656]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((125477,[12393,12333,10409,10324,14407,13245,13346,13737,14076,10802]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((127842,[14329,14392,10219,10306,13485,13348,13440,14149,13285,10514]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((125781,[13406,13895,10439,10480,13443,13464,13300,12709,13731,10531]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((125935,[13675,13917,10153,9631,12886,12982,13765,14454,14194,9896]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0])),((126024,[12985,13143,10184,10567,14100,13332,13099,13602,14211,10398]),([1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0]))])) %%% {-
%\tableRow   {Budgetting          } { 37} {3280} {  0:45   } {  7  } {99/5\speedup{19} & 1239/17\speedup{70} & 751/9\speedup{80}} {  \fromto{1}  {3}     } {    2    }
\tableRow   {Budgetting          } { 37} {3280} {  0:45   } {  7  } {7&\plusminus{0}&\speedup{17} & 13&\plusminus{2}&\speedup{80} & 9&\plusminus{2}&\speedup{80}} {  \fromto{1}  {3}     } {    2    }
 %%% -} next, ("Budgetting",([312,329,317,328,328,348,343,336,311,328],[((46,[8,6,4,7]),([3,2,2,1],[4,2,3,2])),((39,[4,5,4,5]),([3,2,2,1],[3,3,1,3])),((37,[5,5,4,5]),([3,2,2,1],[5,2,4,2])),((64,[12,8,14,5]),([3,2,2,1],[2,5,2,2])),((40,[4,3,7,5]),([3,2,2,1],[2,3,5,2])),((38,[4,5,5,5]),([3,2,2,1],[2,3,3,4])),((36,[4,4,4,8]),([3,2,2,1],[2,2,13,2])),((41,[5,4,9,3]),([3,2,2,1],[5,2,3,2])),((45,[5,5,11,4]),([3,2,2,1],[6,3,3,3])),((49,[7,7,9,4]),([3,2,2,1],[2,2,4,4]))],[((1069,[34,332,347,339]),([3,2,2,1],[72,708,717,334])),((1054,[33,323,347,335]),([3,2,2,1],[66,718,712,351])),((1192,[36,337,441,332]),([3,2,2,1],[75,723,704,496])),((1133,[91,349,335,326]),([3,2,2,1],[84,734,713,330])),((1042,[30,333,326,331]),([3,2,2,1],[71,694,913,330])),((1045,[32,329,328,334]),([3,2,2,1],[73,697,680,329])),((1049,[37,331,333,332]),([3,2,2,1],[80,698,687,760])),((1084,[34,345,350,336]),([3,2,2,1],[86,720,718,374])),((1152,[33,369,355,368]),([3,2,2,1],[86,769,723,359])),((1157,[37,382,349,363]),([3,2,2,1],[101,753,715,344]))])) %%% {-
%\tableRow   {States Table A\hasvideo{}} { 37} {3037} {  2:36   } {  11 } {14460/50\speedup{200} & 39690/216\speedup{180} & 20044/85\speedup{200}} {  \fromto{1}  {2}     } {   1.18  }
\tableRow   {States Table A\hasvideo{}} { 37} {3037} {  2:36   } {  11 } {57&\plusminus{5}&\speedup{200} & 154&\plusminus{20}&\speedup{200} & 85&\plusminus{20}&\speedup{200}} {  \fromto{1}  {2}     } {   1.18  }
 %%% -} next, ("States Table A\\hasvideo{}",([312,280,280,272,280,321,359,297,328,308],[((1407,[148,79,87,96,67,68,78,75,60,56,55]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,1,0,0,0,0,60,0,0,0])),((1343,[72,85,77,103,77,88,72,60,54,67,59]),([1,1,2,1,1,1,1,2,1,1,1],[1,0,1,0,0,0,1,73,0,0,0])),((1448,[71,77,85,108,84,77,82,82,63,70,71]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,1,0,0,0,0,66,0,0,0])),((1460,[74,105,180,88,88,65,62,70,50,67,53]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,1,0,0,0,1,54,0,0,0])),((1446,[79,66,92,90,62,66,107,67,55,112,51]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,4,0,0,0,0,57,0,0,0])),((1240,[62,73,64,105,69,65,70,64,54,60,54]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,1,0,0,0,0,55,0,0,0])),((1551,[83,73,215,98,79,77,79,69,57,55,65]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,1,0,0,0,0,64,0,0,0])),((1376,[64,74,74,108,117,76,69,73,67,64,54]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,0,0,0,0,0,97,0,0,0])),((1427,[70,67,68,99,76,77,77,81,61,68,66]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,0,0,0,0,0,63,0,0,0])),((1710,[74,84,76,115,85,86,97,98,70,72,129]),([1,1,2,1,1,1,1,2,1,1,1],[0,0,1,0,1,0,0,58,0,0,0]))],[((214865,[22357,21672,21166,33782,23408,15568,15218,14602,15562,15728,15256]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((213512,[23131,21638,21535,35043,21480,14460,14943,15405,15124,14802,15481]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,1,0,0,0])),((212987,[22064,21588,21753,34513,23211,14980,15694,14708,14597,14906,14486]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((211254,[20619,20870,21390,34149,21563,14710,15764,15121,15260,15923,15355]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((222724,[22937,22401,23717,37343,23315,15781,15477,15090,15604,15028,15536]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((223547,[22388,22590,22433,34660,24095,15530,16754,16012,16416,16403,15775]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((229442,[23103,23096,23970,35841,23355,16379,16413,17323,16602,16412,16419]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((219237,[21950,22345,22125,33636,22162,16425,15456,15520,17042,16132,15972]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((228683,[23125,22993,23758,35173,23771,15959,15900,17271,16654,16171,17407]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0])),((233524,[22544,23334,23884,39690,24116,16304,16899,16285,16288,17162,16507]),([1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0]))])) %%% {-
%\tableRow   {States Table B\hasvideo{}} {126} {7738} {  0:43   } {  7  } {229955/179\speedup{1200} & 285097/533\speedup{500} & 258168/331\speedup{700}} {  \always{1} } {1        }
\tableRow   {States Table B\hasvideo{}} {126} {7738} {  0:43   } {  7  } {256&\plusminus{40}&\speedup{900} & 456&\plusminus{50}&\speedup{600} & 331&\plusminus{80}&\speedup{700}} {  \always{1} } {1        }
 %%% -} next, ("States Table B\\hasvideo{}",([854,771,740,764,903,805,672,746,662,821],[((2422,[302,241,179,218,233,204,228]),([1,1,1,1,1,1,1],[16,2,0,0,0,0,0])),((3467,[456,338,279,346,323,290,330]),([1,1,1,1,1,1,1],[4,3,0,0,0,0,0])),((3671,[471,391,235,323,287,279,505]),([1,1,1,1,1,1,1],[17,2,0,0,0,0,0])),((3780,[456,300,364,371,463,305,320]),([1,1,1,1,1,1,1],[11,4,0,0,0,0,0])),((3210,[445,271,305,315,314,269,293]),([1,1,1,1,1,1,1],[3,2,0,0,0,0,0])),((2978,[472,218,344,227,235,225,361]),([1,1,1,1,1,1,1],[3,2,0,0,1,0,0])),((2755,[374,222,258,253,245,204,381]),([1,1,1,1,1,1,1],[2,7,0,0,0,0,0])),((4250,[485,411,477,333,358,331,533]),([1,1,1,1,1,1,1],[11,3,0,0,0,0,0])),((3837,[488,330,410,394,334,341,309]),([1,1,1,1,1,1,1],[14,3,0,0,0,0,0])),((3321,[460,277,229,361,306,269,293]),([1,1,1,1,1,1,1],[2,3,0,0,0,0,0]))],[((1796129,[256831,237831,239999,251981,263528,266221,278821]),([1,1,1,1,1,1,1],[0,0,0,0,0,0,0])),((1802780,[275643,251086,246281,250192,261167,262932,254613]),([1,1,1,1,1,1,1],[0,1,0,0,0,0,0])),((1797307,[270422,253209,236777,255894,259961,263479,256649]),([1,1,1,1,1,1,1],[0,0,0,0,0,0,1])),((1790487,[261676,237915,242720,256272,258132,269162,263768]),([1,1,1,1,1,1,1],[0,0,0,0,0,0,0])),((1818333,[273493,231801,243731,256688,273219,276948,261423]),([1,1,1,1,1,1,1],[0,0,0,0,0,0,0])),((1821589,[276986,251160,256197,263122,258286,256711,258245]),([1,1,1,1,1,1,1],[0,0,0,0,0,0,0])),((1829612,[281994,238903,255767,251291,255218,270087,275508]),([1,1,1,1,1,1,1],[0,0,0,1,0,0,0])),((1810368,[285097,237025,252708,260753,262475,260315,251123]),([1,1,1,1,1,1,1],[0,0,0,1,1,0,0])),((1811179,[284701,251651,257345,253496,260049,260509,242651]),([1,1,1,1,1,1,1],[1,0,0,0,0,1,0])),((1802826,[280963,229955,247860,262568,257177,260723,262658]),([1,1,1,1,1,1,1],[0,0,1,0,0,0,0]))])) %%% {-
%\tableRow   {Markdown            } {128} {11786} {  2:08   } {  6  } {18410/1241\speedup{14} & 23715/2219\speedup{10} & 21775/1607\speedup{13}} {  \always{1} } {1        }
\tableRow   {Markdown            } {128} {11786} {  2:08   } {  6  } {1369&\plusminus{90}&\speedup{14} & 1889&\plusminus{150}&\speedup{12} & 1607&\plusminus{200}&\speedup{13}} {  \always{1} } {1        }
 %%% -} next, ("Markdown",([1133,985,1031,1147,1129,1320,1282,1196,1199,1364],[((15346,[1897,1793,1734,1566,1967,2042]),([1,1,1,1,1,1],[3,0,0,0,0,0])),((14655,[1609,1823,1674,1462,1862,1972]),([1,1,1,1,1,1],[2,0,0,0,0,0])),((13510,[1692,1519,1282,1495,1851,1812]),([1,1,1,1,1,1],[1,0,0,0,0,0])),((13180,[1404,1671,1445,1438,1473,1694]),([1,1,1,1,1,1],[1,0,0,0,0,0])),((12702,[1333,1762,1653,1299,1489,1767]),([1,1,1,1,1,1],[1,0,0,0,0,0])),((14248,[1404,1707,1618,1485,1790,2219]),([1,1,1,1,1,1],[0,0,0,0,0,0])),((13132,[1376,1570,1409,1384,1914,1803]),([1,1,1,1,1,1],[1,1,0,0,0,0])),((12619,[1345,1631,1428,1453,1305,1723]),([1,1,1,1,1,1],[1,0,0,0,0,0])),((12821,[1415,1766,1397,1342,1752,1506]),([1,1,1,1,1,1],[2,0,0,0,0,0])),((12906,[1239,1528,1419,1404,1647,1939]),([1,1,1,1,1,1],[2,0,0,0,0,0]))],[((123177,[21748,19298,21113,18553,19345,19841]),([1,1,1,1,1,1],[1,0,0,0,0,0])),((130278,[18407,20495,20792,21399,21949,23627]),([1,1,1,1,1,1],[3,0,0,0,0,0])),((134502,[20948,22318,22800,21193,21250,22286]),([1,1,1,1,1,1],[3,0,0,0,0,0])),((136361,[20976,22017,22239,22833,22437,22017]),([1,1,1,1,1,1],[4,0,0,0,0,0])),((137524,[22180,22371,21823,22868,21687,23132]),([1,1,1,1,1,1],[8,0,0,0,0,0])),((137319,[22657,22724,22181,21961,21884,22219]),([1,1,1,1,1,1],[3,0,0,0,0,0])),((138610,[21320,21655,22849,23050,23547,22718]),([1,1,1,1,1,1],[2,0,0,0,0,0])),((137036,[22677,22543,22165,23715,21484,20512]),([1,1,1,1,1,1],[4,0,0,0,0,0])),((136512,[23143,21356,21060,21368,22937,22494]),([1,1,1,1,1,1],[3,0,0,0,0,0])),((131665,[21786,22564,21863,19929,20968,21176]),([1,1,1,1,1,1],[3,0,0,0,0,0]))])) %%% {-
%\tableRow   {Recipe\hasvideo{}} {193} {14545} {  3:51   } {  17 } {13301/194\speedup{60} & 44063/2783\speedup{15} & 21741/1328\speedup{16}} {  \fromto{1}  {2}     } {   1.05  }
\tableRow   {Recipe\hasvideo{}} {193} {14545} {  3:51   } {  17 } {243&\plusminus{30}&\speedup{60} & 2237&\plusminus{200}&\speedup{16} & 1328&\plusminus{500}&\speedup{16}} {  \fromto{1}  {2}     } {   1.05  }
 %%% -} next, ("Recipe\\hasvideo{}",([1457,1478,1447,1494,1527,1638,1352,1365,1367,1420],[((23960,[1452,1230,1299,228,864,1578,1003,1678,795,827,818,1847,963,1048,1251,970,1030]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1068,1,0,0,0,0,0,0,0])),((23303,[1438,1324,1216,244,781,1599,958,1648,825,894,1010,1735,1071,961,950,1040,966]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1111,0,0,0,0,0,1,0,0])),((25222,[1710,1360,1449,194,837,1719,783,2028,890,1047,859,1762,1302,1027,1062,1212,994]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,1,0,0,0,1,0,1337,0,1,0,0,0,0,0,0])),((24420,[1504,1314,1328,251,1030,1594,834,2074,830,875,856,1923,957,991,985,1234,1074]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,1,1,0,0,0,0,0,1096,0,0,0,0,0,0,0,0])),((25931,[1406,1295,1449,199,836,1814,909,2095,823,853,984,1822,1516,1068,1114,1201,1457]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1055,0,0,1,0,0,0,0,0])),((30149,[1718,1599,1705,234,1061,1769,1001,2071,922,1055,1195,2783,1527,1412,1348,1336,1190]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,1,0,0,0,0,0,1320,8,1,0,0,0,0,0,0])),((27867,[1726,1745,1380,227,1129,1755,1066,2073,1009,896,866,2187,1321,1132,1131,1446,1131]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,1,0,0,0,0,1,1331,0,1,0,0,0,0,0,0])),((30425,[1692,1783,2136,335,1102,1975,981,2138,965,1257,907,2198,1267,1436,1166,1462,1074]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,1,0,0,0,0,0,0,1296,0,1,0,0,0,0,0,0])),((29655,[1476,1466,2081,262,1092,1915,1037,2234,993,1437,1063,2639,1167,1360,1525,1064,1147]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1735,0,1,0,0,0,0,0,0])),((26393,[1702,1581,1298,256,1062,1691,834,1884,848,1116,1042,1900,1320,1187,1033,1106,998]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,1212,1,0,0,0,0,0,0,0]))],[((281198,[18791,16690,16743,14040,15995,16878,14811,18313,14555,14725,16192,17975,15752,16686,17174,15233,15905]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,13097,0,0,0,0,0,0,0,0])),((279060,[16269,16495,17117,13301,15139,16730,14699,17600,16114,14996,14617,17292,15886,17098,17221,17394,16550]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,12973,0,0,0,0,0,0,0,0])),((381483,[19444,22674,22434,19449,22216,25151,22786,23867,20322,24641,21720,23702,20788,20346,21794,22154,22239]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,18574,0,0,0,0,0,0,0,0])),((374187,[21074,18742,19218,16525,20561,23619,23651,24598,22595,21269,21123,23880,23198,21799,22712,21737,22486]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,19979,0,0,0,0,0,0,0,0])),((341853,[20811,19544,22881,16549,19124,21655,19585,21622,18519,19846,17988,20965,19113,20012,19624,19661,18487]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,17372,0,0,0,0,0,0,0,0])),((365213,[23725,23044,24484,17879,20399,25591,22851,23001,21753,22318,22140,22731,19410,15469,16498,18260,19571]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,18691,0,0,0,0,0,0,0,0])),((358696,[21620,21523,22644,17129,20157,21526,19945,25416,21803,20367,21803,21331,19203,21074,19878,17429,19880]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,16805,0,0,0,0,0,0,0,0])),((402933,[21222,23076,25959,19790,23988,25425,23583,28199,20807,25436,24921,25037,23886,21521,20449,21661,21790]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,19143,0,0,0,0,0,0,0,0])),((391851,[21664,21424,23174,20535,23036,26399,21074,24937,23680,21161,25907,27141,21209,21774,21170,19467,21989]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,18464,0,1,0,0,0,0,0,0])),((399650,[21499,23400,20678,18032,21230,27248,23588,24742,22924,25703,23571,24453,24417,23790,22984,23451,22161]),([1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,21139,0,0,0,0,0,0,0,0]))])) %%% {-
-- END OF PASTE
 %%% -} next
 ]


programs = Dict.fromList [
  ("States Table A\\hasvideo{}", ExamplesGenerated.tableOfStatesA),
  ("States Table B\\hasvideo{}", ExamplesGenerated.tableOfStatesB),
  ("States Table C", ExamplesGenerated.tableOfStatesC),
  --("Markdown Recursive", ExamplesGenerated.fromleo_markdown),
  --("Markdown Linear",    ExamplesGenerated.fromleo_markdown_optimized),
  ("Markdown",    ExamplesGenerated.fromleo_markdown_optimized),
  --("Markdown with lens", ExamplesGenerated.fromleo_markdown_optimized),
  --("Markdown w/o lens", ExamplesGenerated.fromleo_markdown_optimized_lensless),
  ("Recipe\\hasvideo{}", ExamplesGenerated.fromleo_recipe2),
  ("Budgetting", ExamplesGenerated.fromleo_conference_budgetting),
  ("Linked-Text", ExamplesGenerated.fromleo_linkedtexteditor),
  ("MVC", ExamplesGenerated.fromleo_modelviewcontroller),
  ("Translation", ExamplesGenerated.fromleo_translatabledoc)
  ]

type OutTransform =
  StringTransform String (String -> String) | ValTransform String (Val -> Val) | HTMLTransform String (String -> String)

type Transform = NoTransform
  | OutputTransform OutTransform
  | ProgTransform (String -> String)
  | SetNextChoice Int
  | TestOutputContains String (Val -> String)

type Benchmark = BUpdate Int String (List Transform)

replaceByMatch: HowMany -> ((String -> String) -> a) -> String -> (Regex.Match -> String) -> a
replaceByMatch howmany final r replacement =
  final <| Regex.replace howmany (regex r) replacement

replaceBy: HowMany -> ((String -> String) -> a) -> String -> String -> a
replaceBy howmany final r result =
  replaceByMatch howmany final r (\_ -> result)

replaceStringBy: String -> String -> Transform
replaceStringBy r replacement = replaceBy (AtMost 1) (StringTransform (r ++ "->" ++ replacement) >> OutputTransform) r replacement

replaceHtmlBy: String -> String -> Transform
replaceHtmlBy r replacement = replaceBy (AtMost 1) (HTMLTransform (r ++ "->" ++ replacement)>> OutputTransform) r replacement

replaceHtmlByReg r replacement = replaceByMatch (AtMost 1) (HTMLTransform (r ++ "->") >> OutputTransform) r replacement

replaceMultiple: String -> List Transform -> Transform
replaceMultiple msg transforms =
  OutputTransform <| ValTransform msg (\input ->
    List.foldl (\t v ->
      case t of
        SetNextChoice _ -> Debug.crash "Cannot invoke SeteNextChoice in a replaceMultiple"
        TestOutputContains _ _ -> Debug.crash "Cannot invok TestOutputContains in a replaceMupliple"
        NoTransform -> v
        OutputTransform to ->
          applyTransform to v
        ProgTransform _ ->
          Debug.crash "Cannot invoke ProgTransform in a replaceMultiple"
    ) input transforms
  )

replaceStringAllBy r replacement = replaceBy All (StringTransform (r ++ "->" ++ replacement)>> OutputTransform) r replacement
replaceHtmlAllBy r replacement = replaceBy All (HTMLTransform (r ++ "->" ++ replacement) >> OutputTransform) r replacement

replaceProgBy = replaceBy Regex.All ProgTransform

transform_markdown_ab_linear = [ NoTransform
   , replaceHtmlBy "demo" "showcase"
   --, replaceHtmlBy "fr\\." "en."
   , replaceHtmlBy "bidirectional" "two-directional"
   , replaceHtmlBy "regex" "_regex_"
   , replaceStringBy "\\[\\s*\"TEXT\",\\s*\" showcase\"\\s*\\]\\s*\\]\\s*\\],"
                     "  [    \"TEXT\",    \" showcase\"      ]      ]      ],[\"div\", [], [[\"TEXT\", \"Hello\"]]],"
   , replaceHtmlBy "Do not use CTRL\\+V" "Do not use CTRL+V</li><li>But use everything else"
   , replaceHtmlBy " to introduce" " <br>to introduce"
   --, replaceHtmlBy "</code>" "</code></li><li>Add a numbered point"
   --, replaceHtmlBy "h4" "h2"
   ]

transform_markdown_ab_lens = [ NoTransform
  , replaceHtmlBy "demo" "showcase"
  , replaceHtmlBy "fr\\." "en."
  , replaceHtmlBy "bidirectional" "two-directional"
  , replaceHtmlBy "regex" "_regex_"
  , replaceHtmlAllBy "list" "lists"
  , replaceHtmlBy " to introduce" " <br>to introduce"
  , replaceHtmlBy "h4" "h2"
  ]

click_on_plus capital state =  [NoTransform
  , replaceHtmlByReg ("("++capital++".*(\r?\n).*(\r?\n).*)style='color:gray'") (\m -> at m.submatches 0 ++ "style='color:coral'")
  , replaceHtmlByReg ("has-been-clicked='( ?)False'(.*(\r?\n).*(\r?\n).*"++state++")") (\m -> "has-been-clicked='"++at m.submatches 0 ++ "True'" ++ at m.submatches 1)
  ]

transform_table_of_states_b = [NoTransform
  , replaceMultiple "Click on the last + button" <| click_on_plus "Hartford" "Connecticut"
  , replaceMultiple "Click on the last generated + button" <| click_on_plus "\\?, \\?" "\\?"
  , replaceHtmlBy "\\?" "Delaware"
  , replaceHtmlBy "\\?" "Dover"
  , replaceHtmlBy "\\?" "DE"
  , replaceHtmlBy "\\?" "Florida"
  , replaceHtmlBy "\\?, \\?" "Tallahassee, FL"
  ]

at l n = Utils.nth l n |> Utils.fromOk "at" |> Maybe.withDefault ""

benchmarks: List Benchmark
benchmarks = [
  {--
  BUpdate 0 "Translation" [NoTransform
      , replaceHtmlBy "printer" "{printer}"
    ],
  --}
  --{--{--{--{--

  {--}
  BUpdate (2*60+36) "States Table A\\hasvideo{}" [ NoTransform
    , replaceProgBy "\"Alabama\", \"AL?\", \"\"" "\"Alabama\", \"AL?\", \"Montgomery\""
    , replaceProgBy "\"Alaska\", \"AL?\", \"\"" "\"Alaska\", \"AL?\", \"Juneau\""
    , replaceHtmlBy "AL\\?" "AL"
    , replaceHtmlBy "AL\\?" "AK"
    , replaceHtmlBy ", AR" "Phoenix, AZ"
    , replaceProgBy "\\+ *\", \"" "+ Update.freeze \", \""
    , replaceMultiple "The last four states" [NoTransform
        , replaceHtmlBy ", AR\\?" "Little Rock, AR"
        , replaceHtmlBy ", CA\\?" "Sacramento, CA"
        , replaceHtmlBy ", CO\\?" "Denver, CO"
        , replaceHtmlBy ", CO\\?" "Hartford, CT"
        ]
    , replaceHtmlBy "lightgray" "yellow"
    , replaceHtmlBy "yellow" "lightblue"
    , replaceHtmlBy "lightblue" "lightcoral"
    , replaceHtmlBy "lightcoral" "lightgoldenrodyellow"
    , replaceHtmlBy "padding:3px" "padding:3px; background-color:orange"
    , replaceHtmlBy "background-color:orange" "background-color:orangered"
    , replaceHtmlBy "background-color:orangered" "background-color:orange"
  ],
  --}
  {--}
  BUpdate (0*60+43) "States Table B\\hasvideo{}" transform_table_of_states_b,
  --}
  {--}
  BUpdate (3*60+51) "Recipe\\hasvideo{}" [ NoTransform
    , replaceHtmlBy " alt='cupcakes'" " alt='cupcakes' style='\\n  float:  right;  \\n:  '"
    --, TestOutputContains "float:  right" valToHtml
    , replaceHtmlBy "Chocolate almond cakes"        "Chocolate Almond Cupcakes"
    --, TestOutputContains "Chocolate Almond Cupcakes" valToHtml
    , replaceStringBy "\\[\\s*\"h1\",\\s*\\[\\],\\s*\\[\\s*\\[\\s*\"TEXT\",\\s*\"Chocolate Almond Cupcakes\""
                       "[ \"h1\", [ [ \"style\", [ [ \"\\n  font-family\", \" cursive\" ], [ \" \\n\",\" \"]]] ], [ [ \"TEXT\", \"Chocolate Almond Cupcakes\""
    , replaceHtmlBy "border:4px solid black;padding:20px" "border:4px solid black;padding:20px;background-color:chocolate"
    , replaceHtmlBy "x='1000'(?=.*\\r?\\n.*\\r?\\n.*Halve)" "x='500'"
    --, TestOutputContains "10 small" valToHtml
    , replaceHtmlBy "melted chocolate\\s*</li>" "melted chocolate</li><li>_10_g of chocolate chip_10s_</li>"
    , replaceHtmlBy "10 small" "80 small"
    , replaceHtmlBy "2 cup of" "2 cup_2s_ of"
    , replaceHtmlBy "2 cups of" "2 cup of"
    , replaceHtmlBy "40 small" "30 small"
    , replaceHtmlBy "30g of chocolate" "1g of chocolate"
    , replaceHtmlBy "small cakes" "small cake_1s_"
    --, SetNextChoice 2
    , replaceHtmlBy "x='50'" "x='100'"
    , replaceHtmlBy "x='100'" "x='200'"
    , replaceHtmlBy "x='200'" "x='400'"
    , replaceHtmlBy "x='400'" "x='800'"
    , replaceHtmlBy "x='800'" "x='1600'"
    ],
  --}
  {--}
  BUpdate (0*60+45) "Budgetting" [ NoTransform
    , SetNextChoice 3
    , replaceHtmlBy "-18000" "0"
    , replaceProgBy "sponsors *= *20000" "sponsors = Update.freeze 35000"
    , SetNextChoice 2
    , replaceHtmlBy "3000" "0"
    , replaceProgBy "sponsors = Update.freeze 35000" "sponsors = Update.freeze 29000"
    , SetNextChoice 2
    , replaceHtmlBy "-6000" "0"
    , replaceHtmlBy "-3000" "0"
    ],
  --}
  {--}
  BUpdate (1*60+11) "MVC" [NoTransform
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Increment)" "trigger='#'"
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Increase multiplier to 3)" "trigger='#'"
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Multiply by 3)" "trigger='#'"
    , replaceHtmlBy "51" "27"
    , replaceHtmlBy "1\\+n\\*3" "(1+n)*2"
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Custom Code)" "trigger='#'"
    , replaceHtmlBy "architect" "design"
    , replaceMultiple "dashes in title" [NoTransform
        , replaceHtmlBy "Model-View-Controller" "Model View Controller"
        , replaceHtmlBy "model-view-controller" "model view controller"
       ]
    , replaceHtmlBy "Increment" "+1"
    , replaceHtmlBy "Decrease multiplier to" "Decrease to"
  ],
  --}
  {--}
  BUpdate (0*60+53) "Linked-Text" [NoTransform
    , replaceHtmlBy "P\\(1\\)" "H(1)"
    {--}
    , replaceHtmlBy "H\\(n\\)" "G(m)"
    , replaceMultiple "prove" [NoTransform
       , replaceHtmlBy "we want to prove" "we want to $prove"
       , replaceHtmlBy "need to prove" "need to $prove"
       , replaceHtmlBy "need to prove" "need to $prove"
       , replaceHtmlBy "we can prove" "we can $prove"
    ]
    , replaceHtmlBy "we want to prove" "we want to show"
    , replaceHtmlBy "we want to show" "we want to really show"
    --}
  ],
  {--
  BUpdate (0*60+43) "States Table C" transform_table_of_states_b,
  --}
  --{--{--
  {--}
  --BUpdate "Markdown Recursive" transform_markdown_ab_linear,
  BUpdate (2*60+8) "Markdown" transform_markdown_ab_linear,
  --BUpdate "Markdown with lens" transform_markdown_ab_lens,
  --BUpdate "Markdown w/o lens" transform_markdown_ab_lens,
  --}
  BUpdate 0 "" []
  ] |>
  List.filterMap (\c -> case c of
  BUpdate _ "" _ -> Nothing
  _ -> Just c )


valToHtml: Val -> String
valToHtml oldOut =
  let slate = LangSvg.resolveToRootedIndexedTree Syntax.Elm 1 1 0 oldOut |> Utils.fromOk "html newOut" in
  let html = LangSvg.printHTML False slate in
  html

transformName: OutTransform -> String
transformName x = case x of
  StringTransform name _ -> name
  HTMLTransform name _ -> name
  ValTransform name _ -> name

transformValToString: OutTransform -> Val -> String
transformValToString x = case x of
  StringTransform _ _ -> valToString
  HTMLTransform _ _ -> valToHtml
  ValTransform _ _ -> valToHtml

applyTransform: OutTransform -> Val -> Val
applyTransform replacement oldOut =
  case replacement of
    StringTransform _ replacement ->
      valToString oldOut |> replacement |>parse |> Utils.fromOk "parse newout" |> eval |> Utils.fromOk "eval newout"
    ValTransform _ replacement -> replacement oldOut
    HTMLTransform _ replacement ->
      --let _ = ImpureGoodies.log html in
      let newHTML = replacement (valToHtml oldOut) in
      case HTMLParser.parseHTMLString newHTML of
        Ok [node] ->
          HTMLValParser.htmlNodeToElmViewInLeo (builtinVal "UpdateBenchmarks") node
        Ok nodes -> Debug.crash <| "Expected only one node after applying HTML transformation, got " ++ toString (List.length nodes)
        Err msg -> Debug.crash (ParserUtils.showError msg)

mbcmp: (number -> number -> number) -> Maybe number -> number -> Maybe number
mbcmp f x n = case x of
  Nothing -> Just n
  Just y -> Just (f y n)

mbmin = mbcmp min
mbmax = mbcmp max
mbacc = mbcmp (+)

sToMinutsSeconds: Float -> String
sToMinutsSeconds s = msToMinutsSeconds (s * 1000)

msToMinutsSeconds: Float -> String
msToMinutsSeconds ms =
  let s = ceiling (ms / 1000) in
  if s < 60 then "0:" ++ String.padLeft 2 '0' (toString s)
  else toString (floor (toFloat s / 60) )++ ":" ++ String.padLeft 2 '0' (toString (s % 60))

speedup unopt opt =
  let inside = stdDevSignificantDigits (toString (toFloat unopt / toFloat opt)) in
  if inside == "NaNx" then "\\nospeedup" else "\\speedup{" ++ inside ++ "}"

runBenchmark: Benchmark -> (String, Int, List Float, Int, Int, List Float, List Float, List Int)
runBenchmark b = case b of
  BUpdate sessionTime benchmarkname replacements ->
    if benchmarkname == "" then ("", 0, [], 0, 0, [], [], [])
    else
    let finalReplacements = List.filter (\x -> x/= NoTransform) replacements in
    let numberOfUpdates = List.length (List.filter (\x ->
      case x of
         ProgTransform _ -> False
         _ -> True
       ) finalReplacements) in
        -- Returns (List of the total time of the session,
    -- and as many update times as there are replacements
    -- Returns the number of ambiguities encountered, and a list of all the times taken to compute all remaining solutions
    let session: Exp ->  Val -> Int -> Bool -> ((Float, List Float), (List Int, List Float))
        session  progExp oldOut i unopt =
      let sessionName = "Session #" ++ toString i ++ " " ++
         (if unopt then "unoptimized" else "optimized") ++" for " ++ benchmarkname in
      let _ = if not exportMode then ImpureGoodies.log <| sessionName  else "" in
      let (_, _, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, _) =
           Utils.foldLeft
                (progExp, oldOut, [],          [],        [],         [],          [],              1) finalReplacements <|
              (\(progExp, oldOut, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, nextChoice) step ->
           {-if (evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog" |> valToString) /= (valToString oldOut) then
             Debug.crash "invariant failed: the oldOut is not generated by the newOut"
           else-}
           let _ = if not exportMode then ImpureGoodies.log <| "Apply transformation..."  else ""in
           --let _ = ImpureGoodies.log (toString unopt) in
           --let _ = ImpureGoodies.log "Current program:" in
           --let _ = ImpureGoodies.log (unparse progExp) in
           --let _ = ImpureGoodies.log "Current out:" in
           --let _ = ImpureGoodies.log (valToHtml oldOut) in
           --let _ = ImpureGoodies.log ("New modifications: " ++ toString replacement)  in
           case step of
             NoTransform -> Debug.crash "NoTransform should have been removed"
             SetNextChoice n -> (progExp, oldOut, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, n)
             TestOutputContains re vToString ->
               if Regex.contains (Regex.regex re) (vToString oldOut) then
                 (progExp, oldOut, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, nextChoice)
               else
                 Debug.crash <| "The output did not contain '" ++ re ++ "' : " ++ vToString oldOut ++ ", program having generated it:\n " ++ unparse progExp
             ProgTransform p ->
               case parse (p (unparse progExp)) of
                 Err msg -> Debug.crash msg
                 Ok newProgExp ->
                   let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval changed prog" in
                   (newProgExp, realNewOut, updateTimes, realNewOutTime::evalTimes, modifTimes, ambiguities, timeAmbiguities, 1)
             OutputTransform replacement ->
               let replacementName = sessionName ++ transformName replacement in
               --let _ = Debug.log ("oldOut\n" ++ valToString oldOut) () in
               --let _ = Debug.log ("oldOut\n" ++ valToHtml oldOut) () in
               let (newOut, newOutTime) = ImpureGoodies.timedRun <| \_ -> applyTransform replacement oldOut in
               --let _ = Debug.log ("newOut\n" ++ valToString newOut) () in
               --let _ = Debug.log ("newOut\n" ++ valToHtml newOut) () in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString newOutTime ++ "ms. Update with newOut..."  else "" in
               --let _ = ImpureGoodies.log (valToHtml newOut) in
               let (newProgExp_, newProgExpDiffs, updateTime, numAmbiguities, timeAllOtherSolutions) = (if unopt then
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut VUnoptimizedDiffs)
                  else
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.doUpdateWithoutLog progExp oldOut newOut) |> \(x, t) -> case x of
                       Results.Oks (LazyList.Nil) -> Debug.crash <| "No solution for " ++ replacementName
                       Results.Oks (LazyList.Cons (headUEnv, headUExp) _ as ll) ->
                         let (allSolutions, timeAllOtherSolutions) = ImpureGoodies.timedRun <| \_ -> LazyList.toList ll in
                         let numAmbiguities = List.length allSolutions in
                         let nonChangingEnv = LazyList.filter (\(env, e) -> List.all (\(i, x) -> x == VUnoptimizedDiffs) env.changes && e.changes /= Nothing) <| LazyList.fromList allSolutions in
                         case nonChangingEnv of
                              LazyList.Nil -> Debug.crash <| unparse headUExp.val ++ "All solutions of " ++ replacementName ++ " modify the environment or do not modify the expression: " ++ envDiffsToString EvalUpdate.preludeEnv headUEnv.val headUEnv.changes
                              _ ->
                         let (newEnv, newExp) = LazyList.elemAt (nextChoice - 1) nonChangingEnv |> Utils.fromJust_ "LazyList head"
                         in
                         if newExp.changes == Nothing then
                           Debug.crash <| "In " ++ replacementName++ ", expected a change to the expression, got Nothing\n" ++ (transformValToString replacement newOut)
                         else
                          (newExp.val, newExp.changes |> Utils.fromJust,  t, numAmbiguities, timeAllOtherSolutions)
                       Results.Errs msg -> Debug.crash <| msg ++ "Transform =  " ++ replacementName ++ "\n" ++ (transformValToString replacement newOut)  ++ "\n" ++ unparse progExp
               in
               let newProgExp = parse (unparse newProgExp_) |> Utils.fromOk_ in
               --let _ = ImpureGoodies.log (eDiffsToString "" progExp newProgExp newProgExpDiffs) in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString updateTime ++ "ms. Recomputing realOut..."  else "" in
               --let _ = ImpureGoodies.log "new program:" in
               --let _ = ImpureGoodies.log (unparse newProgExp) in
               let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval prog" in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString realNewOutTime ++ "ms."  else "" in
               --let _ = ImpureGoodies.log "Real out:" in
               --let _ = ImpureGoodies.log (valToHtml realNewOut) in
               (newProgExp, realNewOut, updateTime::updateTimes, realNewOutTime::evalTimes, newOutTime::modifTimes, numAmbiguities::ambiguities, timeAllOtherSolutions::timeAmbiguities, 1)
               )
      in
      ((List.sum updateTimes + List.sum evalTimes + List.sum modifTimes, List.reverse updateTimes), (List.reverse ambiguities, List.reverse timeAmbiguities))
    in
    let prog = Dict.get benchmarkname programs |> Utils.fromJust_ "Prog" in
    let (evalTimes, optResults, unoptResults) =
         case Dict.get benchmarkname benchmarkCache of
          Nothing ->
            let (progExp, parseProgTimes) = averageTimedRun nToAverageOn (\_ -> parse prog |> Utils.fromOk "parse prog") in
            let (oldOut, evalProgTimes) = averageTimedRun nToAverageOn (\_ -> evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog") in

            (List.map2 (\x y -> x + y) parseProgTimes evalProgTimes,
             tryMany nToAverageOn <| \i -> session progExp oldOut i False,
             tryMany nToAverageOn <| \i -> session progExp oldOut i True
             )
          Just x -> x
 --           let (progExp, parseProgTimes) = averageTimedRun nToAverageOn (\_ -> parse prog |> Utils.fromOk "parse prog") in
   --         let (oldOut, evalProgTimes) = averageTimedRun nToAverageOn (\_ -> evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog") in
     --       (x, y, List.map2 (\x y -> x + y) parseProgTimes evalProgTimes)
    in
    let allUpdateTimesBySession results = List.map (\l ->
         List.map2 (\firstSolutionTime otherSolutionTime -> firstSolutionTime + otherSolutionTime) (Tuple.second <| Tuple.first l) (Tuple.second <| Tuple.second l)) results in
    let allUpdateTimes results = List.concatMap identity <| allUpdateTimesBySession results in
    let allUnoptTimes = allUpdateTimes unoptResults in
    let allOptTimes = allUpdateTimes optResults in
    -- It's useless to consider ambiguities accross sessions because they are the same. But the times may vary

    --let allTimeAmbiguities = List.concatMap (Tuple.second << Tuple.second) in
    let ambiguities = List.head >> Utils.fromJust_ "ambiguity" >> Tuple.second >> Tuple.first in
    let ambiguitiesOpt = ambiguities optResults in
    --let timeAmbiguitiesOpt = allTimeAmbiguities optResults in
    --let ambiguitiesUnopt = ambiguities unoptResults in
    --let timeAmbiguitiesUnopt = allTimeAmbiguities unoptResults in

    --let onlyRelevantAmbiguities ambiguities timeAmbiguities =
    --  List.filterMap identity (List.map2 (\a t -> if a == 1 then Nothing else Just t) ambiguities timeAmbiguities) in

    --let timeAmbiguitiesOptPruned = onlyRelevantAmbiguities ambiguitiesOpt timeAmbiguitiesOpt in
    --let ambiguitiesUnopt = ambiguities unoptResults in
    --let timeAmbiguitiesUnoptPruned = onlyRelevantAmbiguities ambiguitiesOpt timeAmbiguitiesUnopt in


    let fastestUnopts = unoptResults |> allUpdateTimesBySession |> List.map fastest in
    let fastestUnoptAv = average fastestUnopts in
    let fastestOpts   = optResults |>  allUpdateTimesBySession  |> List.map fastest in
    let fastestOptAv  = average fastestOpts in
    let fastestOptStd = stddev fastestOpts in
    let fastestOpt = fastest allOptTimes in
    let fastestUnopt = fastest allUnoptTimes in

    let slowestUnopts = unoptResults |> allUpdateTimesBySession |> List.map slowest in
    let slowestUnoptAv = average slowestUnopts in
    let slowestOpts   = optResults |>  allUpdateTimesBySession  |> List.map slowest in
    let slowestOptAv  = average slowestOpts in
    let slowestOptStd = stddev slowestOpts in
    let slowestOpt = slowest allOptTimes in
    let slowestUnopt = slowest allUnoptTimes in

    let averageUnopt = average allUnoptTimes in
    let averageOpt   = average allOptTimes   in
    let averagestd   = stddev allOptTimes in

    let minAmbiguity = minimum ambiguitiesOpt in
    let maxAmbiguity = maximum ambiguitiesOpt in
    let averageAmbiguity = average <| List.map toFloat ambiguitiesOpt in
    --let fastestAmbiguityUnopt = if (maxAmbiguity == 1) then 0 else fastest timeAmbiguitiesUnoptPruned in
    --let fastestAmbiguityOpt   = if (maxAmbiguity == 1) then 0 else fastest timeAmbiguitiesOptPruned   in
    --let slowestAmbiguityUnopt = if (maxAmbiguity == 1) then 0 else slowest timeAmbiguitiesUnoptPruned in
    --let slowestAmbiguityOpt   = if (maxAmbiguity == 1) then 0 else slowest timeAmbiguitiesOptPruned   in
    --let averageAmbiguityUnopt = if (maxAmbiguity == 1) then 0 else average timeAmbiguitiesUnoptPruned in
    --let averageAmbiguityOpt   = if (maxAmbiguity == 1) then 0 else average timeAmbiguitiesOptPruned   in

    --let averageUnoptSessionTime = msToMinutsSeconds <| average <| List.map (Tuple.first << Tuple.first) unoptResults in
    --let averageOptSessionTime   = msToMinutsSeconds <| average <| List.map (Tuple.first << Tuple.first) optResults in

    let locprog = loc prog in
    let finalEvalTime = ceiling (average evalTimes)  in
    let stddevEvalTime = stddev evalTimes in
    let latexRow newFormat =
      let displazyunoptoptspeedup timeUnopt timeOpt timesUnoptAv timesOptAv std =
            if timeUnopt < 5 && timeOpt < 5 then
               String.pad 15 ' ' ("\\lessthanms{5}")
            else
              if newFormat then
                String.pad 15 ' ' (s timesOptAv ++ "&\\plusminus{"++std ++ "}"++ "&" ++ speedup (floor timesUnoptAv) (ceiling timesOptAv))
              else
                String.pad 15 ' ' (s timeUnopt ++ "/" ++ s timeOpt ++ speedup timeUnopt timeOpt)
      in
      (if only1 then "\\tableRowToUpdate" else "\\tableRow") ++ "   {" ++
       String.padRight 20 ' ' benchmarkname ++ "} {" ++
       String.pad 3 ' ' (toString <| locprog) ++ "} {" ++
       String.pad 4 ' ' (toString <| finalEvalTime) ++ "} {\\plusminus{" ++ stddevEvalTime ++ "}} { "++
       String.pad 6 ' ' (sToMinutsSeconds (toFloat sessionTime)) ++"  } { "++
       String.pad 3 ' ' (toString numberOfUpdates) ++" } {" ++
       displazyunoptoptspeedup fastestUnopt fastestOpt fastestUnoptAv fastestOptAv fastestOptStd ++ " & " ++
       displazyunoptoptspeedup slowestUnopt slowestOpt slowestUnoptAv slowestOptAv slowestOptStd ++ " & " ++
       displazyunoptoptspeedup averageUnopt averageOpt averageUnopt   averageOpt   averagestd++ "} " ++
       "{  " ++ (if (minAmbiguity == maxAmbiguity)
            then "\\always{" ++ toString minAmbiguity ++ "} } {" ++ toString minAmbiguity  ++ "        "
            else "\\fromto{" ++ toString minAmbiguity ++ "}  {" ++ toString maxAmbiguity ++ "}     } {" ++ String.pad 9 ' ' ((String.left 4 (toString averageAmbiguity)))) ++ "} "
       --"{" ++ (if(minAmbiguity == maxAmbiguity) then
       --     String.pad 15 ' ' "\\noambiguity" ++ " & " ++ String.pad 15 ' ' "\\noambiguity" ++ " & " ++String.pad 15 ' ' "\\noambiguity"
       --  else
       --  displazyunoptoptspeedup fastestAmbiguityUnopt fastestAmbiguityOpt ++ " & " ++
       --  displazyunoptoptspeedup slowestAmbiguityUnopt slowestAmbiguityOpt ++ " & " ++
       --  displazyunoptoptspeedup averageAmbiguityUnopt averageAmbiguityOpt
        --)++ "} "
    in
    let _ = ImpureGoodies.log ("%" ++ latexRow False) in
    let _ = ImpureGoodies.log (latexRow True) in
    let rendersession results = List.map (\((session, upds), (ambiguities, timeAmbiguities)) ->
       "\n% session: " ++ toString session ++ ", updates: " ++
         (List.map toString upds |> String.join ",") ++ ",\n%solutions: " ++
         (List.map toString ambiguities |> String.join ",") ++ " / times: " ++
         (List.map toString timeAmbiguities |> String.join ",")
         ) results |> String.join "" in
    let rawdata = "\n% " ++ benchmarkname ++ " - Unopt" ++ rendersession unoptResults ++
      "\n% " ++ benchmarkname ++ " - Opt" ++ rendersession optResults in
    let result = (rawdata, locprog, evalTimes, sessionTime, numberOfUpdates, allUnoptTimes, allOptTimes,
      ambiguitiesOpt) in
    let _ = ImpureGoodies.log (" %%% -} next, " ++ toString (benchmarkname, (evalTimes, optResults, unoptResults)) ++ " %%% {-") in
    result

header =
  ImpureGoodies.log """
   %%% {-
\\newcommand{\\benchmarks}{
%
% Benchmark Rows
%
% \\tableRow {                    } {   } {    } {     Session     } {  Fastest Upd  } {  Slowest Upd  } {  Average Upd  } { #Amb     }
% \\tableRow {     Example        } {LOC} {Eval} {  Time  } {\\#Upd } {  Unopt / Opt  } {  Unopt / Opt  } {  Unopt / Opt  } { Min/Max}{Average }"""

compute = (Utils.foldLeft
    ("",  0,   [],    0,    0,   [],       [],     [])     benchmarks <|
  (\(acc, loc, eval, time, upd, updUnopt, updOpt, ambiguities) b ->
     let (acc2, loc2, eval2, time2, upd2, updUnopt2, updOpt2, ambiguities2) = runBenchmark b in
     (acc ++ acc2, loc + loc2, eval ++ eval2, time + time2, upd + upd2,
      updUnopt ++ updUnopt2, updOpt ++ updOpt2, (ambiguities ++ ambiguities2) ))
  ) |>
       (\(acc, loc, eval, time, upd, updUnopt, updOpt,ambiguities) ->
         let _ = ImpureGoodies.log acc in
         let unoptaverage = average updUnopt in
         let optaverage = average updOpt in
         let optaveragestddev = stddev updOpt in
         ImpureGoodies.log <| """}
\\newcommand{\\benchmarksloc}{""" ++ toString loc ++ """}
\\newcommand{\\benchmarkslocfloored}{""" ++ toString (floor (toFloat loc / toFloat 100) * 100) ++ """}
\\newcommand{\\benchmarksnum}{""" ++ toString (List.length benchmarks) ++ """}
\\newcommand{\\benchmarksevalaverage}{""" ++ toString (ceiling <| average eval) ++ """}
\\newcommand{\\benchmarksevalstddev}{\\plusminus{""" ++ stddev eval ++ """}}
\\newcommand{\\benchmarkssessiontime}{""" ++ sToMinutsSeconds (toFloat time) ++ """}
\\newcommand{\\benchmarksnumupd}{""" ++ toString upd ++ """}
\\newcommand{\\benchmarksaverageoptupd}{""" ++ s optaverage ++ """}
\\newcommand{\\benchmarksaverageunoptupd}{""" ++ s unoptaverage ++ """}
\\newcommand{\\benchmarksaveragestddev}{\\plusminus{""" ++ optaveragestddev ++ """}}
\\newcommand{\\benchmarksaveragespeedup}{""" ++ speedup (floor unoptaverage) (ceiling optaverage) ++ """}
\\newcommand{\\benchmarkssolutions}{"""++ toString (minimum ambiguities) ++ " to " ++ toString (maximum ambiguities) ++ """}
\\newcommand{\\benchmarkssolutionsaverage}{"""++(String.left 4 (toString <| average (List.map toFloat ambiguities)))++"""}
  %%% -} next
""")




parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> ParserUtils.showError p)
unparse = Syntax.unparser Syntax.Elm
evalEnv env exp = Eval.doEval Syntax.Elm env exp |> Result.map (Tuple.first >> Tuple.first)
eval exp = Eval.doEval Syntax.Elm [] exp |> Result.map (Tuple.first >> Tuple.first)

average: List Float -> Float
average l = List.sum l / toFloat (List.length l)

onlyZeroesBeforeComma: String -> String
onlyZeroesBeforeComma =
  Regex.replace Regex.All (Regex.regex "\\..*") (\_ -> "") >>
  Regex.replace Regex.All (Regex.regex ".") (\_ -> "0")

stdDevSignificantDigits: String -> String
stdDevSignificantDigits sss =
  case String.uncons sss of
      Just ('1', rem) ->
        case String.uncons rem of
          Just ('.', rem2) ->
            case String.uncons rem2 of
              Just (c, _) -> "1." ++ String.fromChar c
              Nothing -> "1"
          Just (d, rem2) ->
            "1" ++ String.fromChar d ++
            onlyZeroesBeforeComma rem2
          Nothing -> "1"
      Just ('0', rem) ->
        case String.uncons rem of
         Just ('.', rem2) ->
           "0." ++ (stdDevSignificantDigits rem2 |> Regex.replace Regex.All (Regex.regex "0*$") (\_ -> ""))
         Just (_, _) ->
           "0" ++ stdDevSignificantDigits rem
         Nothing -> "0"
      Just (first, rem) ->
         String.fromChar first ++ onlyZeroesBeforeComma rem
      Nothing -> "0"

stddev: List Float -> String
stddev l =
  let a = average l in
  let ss = sqrt (List.sum (List.map (\x -> x * x) l) / (toFloat (List.length l)) - a * a) in
  let sss = toString ss in
  stdDevSignificantDigits sss

minimum: List comparable -> comparable
minimum l = List.minimum l |> Utils.fromJust

fastest = minimum

maximum: List comparable -> comparable
maximum l = List.maximum l |> Utils.fromJust

slowest = maximum

s: Float -> String
s f = toString (ceiling f)

tryMany: Int -> (Int -> a) -> List a
tryMany n callback =
  List.reverse <| List.foldl (\i b -> callback i::b) [] (List.range 1 n)

averageTimedRun: Int -> (() -> a) -> (a, List Float)
averageTimedRun n callback =
  List.range 1 n |>
  List.foldl (\_ (oldResult, oldAcc) ->
    let (newResult, newTime) = ImpureGoodies.timedRun callback in
    (Just newResult, newTime::oldAcc)) (Nothing, []) |>
  \(newResult, times) -> (newResult |> Utils.fromJust, times)

loc s = String.lines s |> List.length