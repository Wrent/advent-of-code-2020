package six

object Customs extends App {
  def sumCounts(input: String): Int = {
    sumCnts(input, Set. empty[Char], (cum, curr) => cum.union(curr))
  }

  private def sumCnts(input: String, initialSet: Set[Char], joiningFn: (Set[Char], Set[Char]) => Set[Char]) = {
    input.split("\n\n")
      .map(_.split("\n").map(_.toCharArray.toSet).foldLeft(initialSet)(joiningFn)).map(_.size)
      .sum
  }

  def sumCountsAll(input: String): Int = {
    sumCnts(input, "abcdefghijklmnopqrstuvwxyz".toCharArray.toSet, (cum, curr) => cum.intersect(curr))
  }

  val input = "gqraplu\njmwftidynvozkhe\n\ncbhejsplztndvig\nduzpteclshfgbinvj\nzvbhdtnigplsej\nztsbdixmegljhnvapy\n\nsyrulqjamnixpwz\nyrjinauqxlz\nyrazqnujxli\nzlnorxjiayqhu\n\nn\ngn\nn\n\nyx\noxh\nfx\n\njoewxkpzyvldqfricsn\nxqfsnyevltrmcdpiwz\npvyxdlnzcqwrseif\ncagnvpqwfbzxuyreidl\n\nlqbpai\nquplimxcojkrt\n\nsziyc\nlsvkuhei\nkctsidr\nxkmousi\nsbjnqgafwi\n\nur\nnu\nu\nu\nu\n\nkgehjantorqdumzfwpivlb\ndvziasuthgmplfwnrqok\n\nejo\nzs\n\nfgbmsua\ngimuqbfv\nlfzugbm\nuvbfmgp\niagmbpfhu\n\nbqntdexkfvpwl\nevjblkfxcduyn\ntrmxfbvklynz\nsvginkxflabh\n\ngdtafy\ndtygafo\ndagfyt\nydaftg\ngdatyf\n\nygwkouhpmqseb\nplokghuzxwybeam\nhncokugxmbpsye\nogfkejuvbmyhp\nhmpnlcregoukqysb\n\nqgm\nxhgjcpmt\nelswfzdgvnoyiu\nmgx\nkagbr\n\njxiaofcy\nvrtpbkhles\n\nd\nd\nd\nn\nd\n\nyidoljquknxpeaz\njxevznrpdlkqsiumao\nopilqzajdxnemukr\nhewjdganufopkizxlqb\n\nprwydlbnkhgoqvsxmafeuj\nhwegbjqxrlmfpvysnakduo\n\nntdzifwmhxkcpg\ngmciznhtu\n\nbvrcmgo\ngvbrcom\nmovrbgc\nvrmobgc\n\nuq\nqeu\nyequ\nqpbumck\nnqu\n\nm\nwlgkmze\nm\n\nytazsmxvhpw\nwphytzvax\nzaxwyfthvpc\naphzswtxvyok\n\nxmd\nxmd\n\nq\nu\nu\ny\n\nnrwzuibkxy\nyjekmus\n\niah\nliah\nhapimu\nauhi\nwhtia\n\nwaxngytpedkcohu\nnicsaypkvztwujleofxrh\nqhopntywekxabuc\ncpawenqyokthxu\n\nztpgmojkuasni\ntkgipajonszum\nsnzpkiaugmojt\nuzniostakjpgm\n\nuvlxwrgyb\nunywtravxlg\nfrglqxuiwy\n\nhnybo\nnhyo\nonhy\n\nqbiayve\ntbmrhxc\n\nifgubhacvkwndempsqyto\noeikgsmwvathyfqupzdnrb\naopbwqgtxfeikhvsdnumy\n\nsdcoulxfwkyh\nylshkufxod\nkdptsfuohvxly\n\nlpinkvcmbjgfzstoyr\nyonrjzpktmfbisc\nifkzmcopsrybtjn\nzscptibfykromjn\n\ndjfzwn\nlsvbyqth\ncfxed\ndnzgw\n\nukbx\nbh\n\nmrxewvdultgp\npkwvlgjtmxid\n\npax\nxa\nrjlxa\naxp\n\nblpq\nuyfaqwbpls\nlnbqp\nqplbzn\nbnqpl\n\nviezwmdhxnbpcuq\nsnwlzhdycvbikqpu\n\nxjoarpudhzcqvbklsf\npuqhzfxcraobkvsdl\nsvalkqorzuhxndcpfb\nzqhkvusbcoalprfdx\n\nqrav\nqrav\navqr\navqr\nvqar\n\nmgizycqbaksjofwtdphn\nqshodcwfbygpmtikjna\ngbhdawmjyofsqtnckpi\nmgufwjpithybodqscnak\n\ndrkwnmigxc\nxndwkrmi\nkdxranwim\nkrxdwiynm\n\nnzvgidp\ndzvpig\nvzgdp\nvpgdzh\npvzgd\n\nowzvyrsqlaefxut\nafslqrywvueoztx\nwzxqartvuioyelsf\nevqlwzxustarfyo\nzetoqsulxyfvawr\n\nrvydefukzonxmshw\nyhxenwmdvsfz\nhnzwxivsmdfbey\n\nhzqkpmrgeldniwo\nnelihzrkpwmgdoq\nzhdixrmpownegqbkl\nlpjkgzhnurwymodesiq\nhrgevlonqmbcdzikwp\n\nzmxskldyfobp\nefwzjdyok\nzyihvutofadk\ndokzeryfgc\n\ninbs\nbsni\nsnib\nsbni\ninkbs\n\nkuhimvsfetnobjayzrxpclq\nmpyqbtfncerkujzlivaxhos\nkhxmojcyuvpqrbitlafzesn\nyjckvrfontsphzabmleuixq\nlhpjmaefbnkyciuortsqxzv\n\nzrlyvkgwsinbteaxcuqdmf\nsqnagelwkbydrxtmufcizv\ndicxqvlgoenabwrtkufsymz\nexvqrkmulnsayzbwdcfgti\nvkgulsxywafinecrbtzdmq\n\nwhpx\nhwpxtme\nnwrvhpx\nhpwx\nrwxhp\n\nq\nvz\nq\nx\n\nwyaum\nouzj\n\nnrqsahcif\nbnia\nnzia\n\nneditjrwlm\nncovfirzdl\nqrhabpxkus\n\nypbtzriv\ndsgj\nbfk\nnq\n\nxuegjwzf\nxbjonevumgf\n\nstxl\ntsl\nlstyf\n\nvcpja\npjacv\ncvajp\njyacpv\n\nhurbisxv\nvrusibhwx\nkhxiqsbturgl\nszbahofeinjurx\n\ncdgbs\nsmpb\n\nnxapgmhuise\nhlgafojqykxpwuv\nrbhigaceuxmp\n\nia\nia\nia\nia\nai\n\nromdclz\nsbpyaqjeicfxwv\nogdhrcun\ndklct\n\nbwpjlzmakrhtgcyd\nlywtdgmrpzahkcj\nzcywdpokjahmgrlt\nthkcypzljgfadrmw\nahtdmzlrygpjfkcw\n\nlqozhpxswn\naegvhkjo\n\nmvrhzutxkewbyfdnqjgoics\nefnzyojvstdhgqrcxwukbim\n\neiqctujxmbovyapflhsnrd\namlunxrvdkpswzfcyoebqjti\n\nftmj\nfxntmhgzyva\nrpamxuvse\ntrsxyfbg\nwdcqkoli\n\nxsovbqdiaztnm\nmjospzaqdcbvlyixn\nobdqizsmavnx\n\nlyqonstrvwk\noswnltrkqvy\noqvlnkyswtr\n\npzyxdkcb\nmsuyp\n\njbtfnimyhgeau\nubjenfatihg\nipheugjnatbf\n\nbohtinfxd\nwhiofqdx\nifjgmrdeco\n\ni\ni\ni\nji\ni\n\nojvu\njuv\nujv\njvu\nujvn\n\nqvjyraeugi\njaigryeuqv\nauirgyvejq\n\nl\nl\nl\nl\n\nwcvneqijzlu\njpfmawgnrts\ntndmxhwjo\n\nikybgcsdhfjqewramz\nzdyiuchgwkreqbjafxp\n\nhwxbvmodf\nvwfmbxdoh\nbovxmfdwh\nhofvwmdbx\nwodvfbmxh\n\nkvqer\nkverq\nvqrke\nkevqrmg\n\nlmcprdquzfojbwg\nuzdljoqrphgcbfm\nrpmnvlucjgzbfdqox\n\nanevrywoscfgmd\npkorawscmf\nrcumfipoaws\n\nudfjiq\nurqvjitfd\njfduqi\n\nal\nl\n\nhqmtedniw\ntnqdihmwez\n\ny\nqy\ny\nley\n\nph\nhp\n\nczogpbqrswyemujkdxvh\nhbycpwtxueksovzrdgmnqj\nukoegrcvsbpwqymdjhxz\n\nihly\nnyd\nvydw\n\nsjvkuolzixacqnbfm\nmilvqkuzfaxbjcnos\n\nvptqewkl\n\njitqrhcebp\nhmdjfebzynqoic\njehbiqsxcvw\n\nyvejfgpdhwr\nsclkjzqgdaitbx\n\nmolycdpuqtezsaikfgb\nrawhmiuxgbsjkqpftzceyldo\nykpfmgcadezvsqtuboil\n\ncfy\nfyc\nfycjz\nycf\n\nnvgokp\ndvlrym\nvbxq\nviec\n\ntshpkilmfn\nrahptloqnmwze\nhcdtgmlpjn\n\nwarkbezshofylgpvc\nsmfyaicelhvpk\nchpnfkyeaxqvsluj\nhcqpatlmdvsfkyei\n\nmdvxgrpbnaeohkitc\ncmkixqptgeo\nmflsxckyzgjpuoe\npmdcxqevkorg\n\njsqdlxcmrnkahgwipfeuz\nzmbjurenytxsaglqpvfwhkic\nipxjszkgrwdhufaqcelnm\nkelwzuhjsmxngripqfcda\n\nqjeilf\nli\nmswnadl\n\nedlsrkoungwibthcp\nskwbtjpcervoldi\nfseklbitcpomwqr\nktcprsbzweihol\n\nhtdqfkovwjyezu\nthobwyfzuldvjem\nyvduezhfjtow\neyhwunjzkdvfto\nfhejnzdytovwixu\n\nfwhtnj\nhjwfntl\nftujnwh\nhbtwjnf\n\nt\ne\nt\nt\nz\n\nnqxcusyotzefvph\nyeocxkiutgmsqf\nckjyemquxsofwtgd\n\ntmzfevydagckuhqlx\nbyvufxtmcrlaqzed\nupctzjfxaleysmdvq\nevcxlnmfdtuyqza\ntqoecfdzivxmuyabl\n\nshgxmojtwil\ndzkqounrelcxw\nbwfyohxl\n\nmcvryskujtwfegnqzpoabl\nkqviphnbclosfegjw\n\ncm\ncm\nicm\n\nbgaxzljdmnikfwty\ninflydazkjwrvmcb\nmblkduehjyaqziwfn\nifbmwdjznyxgakol\nimfdlwabhkynzj\n\njlrwutvxeh\njlrdwhtvxeu\nrjkhluwetxpv\n\nlwikdzhvmc\ncyredwfnkizs\n\ngxb\nx\nx\n\nyeguzh\nzgh\nhpzc\n\npaglzktcsyewuvboq\ncgloauxwzvqsyktepb\n\ny\nm\nb\nb\n\njogqmp\nimogpj\nmgnpjoa\n\nicgwq\ncwig\n\npiyuamfk\nidyufmopea\nacfuzgniypm\n\nipkmh\nibhklmp\nphimk\ntihpkm\npcvkhuzmyni\n\njugsl\nlujh\nluyirmj\nljgsu\njlu\n\ngzbesjik\npzisbjekg\nzjksigbe\n\nmdp\nmdcp\nomd\nxqmwcdp\nikrfdyjlm\n\nwgdjmfbrntluzkysipq\npfiknrqtbzglyuwjsmd\n\nwgbfixvksaejo\njph\n\nwtsyo\nysw\nqyl\n\nulsbickanygpxqwzhmjf\nfaqugxicyljwpzhsnkbm\n\nzhrdfgxw\nxgzqfmd\nflkjdgxz\nyzfcbdxvneg\nndxfgzo\n\nilkpncyqfbvorhtwexjds\niaebcyqudfprh\nmqefrdbhcpyi\ncdrhypiefbgq\n\nzefqwirsglpjv\nhdivwsnqb\n\nvzcexgstnfk\nfarxkgzcsbnevt\nsfczvtgnxke\ntxepoknzgsvfc\nksnevtfxcgz\n\nyrlnqwvpmeiuxt\nxqpirlhnymowuv\n\nxan\njpfa\naczoup\ncja\naibgwevkh\n\nm\nsml\nopu\n\nrdbchfwvyamizs\nbofxszcamyrjdh\nrzsntcpmalgyh\n\nftevgjrbkxmnuidsaqz\ngexasrutkivfbdzqnjm\ndnimrtkeugfzajsxvqb\nquaxkivgnjdrtfbzems\nqxriuendmbagfjzvkst\n\nwthspyrxaijgbkzumvolqe\novrgyukexzmpqjhflwit\ntizgwxorvepjkyhlqum\nurlwhtzkvjdqgoximype\nrevuqwpogzimyhlxtkj\n\nedpukhotxswnb\ndstnhbueokwpx\n\nyhqwopgnutivk\nqiotnupkvyw\n\nznjofavtsgrkh\njvahkzotsfgnr\nvhfnrsgtkzoaj\ngfathnzrkosjv\ntvzfsrkjohnga\n\nevyutzwhmxdk\nzxrmsqvfbiwojpg\n\nre\nktrhjfiz\nc\naldugvpqybnx\n\nwaexnqfbrul\nertydvxu\nurxe\nechxuor\nrxeu\n\nqtnxbpklyjiw\ndmvocgqbp\nberqpiuz\nhrbkzqp\njrypbsqa\n\nbrhqpmziaxfgle\nrwuzlfenmihpg\nlfcmprhazeig\n\nsuzhwbrpe\nclpusebrwai\npbeasgrofnwu\ndvjwstpxqerkymub\n\nkbouvjnqaysmgrhtwp\nszdvbjhwpxygunfomkt\ngtkleonjbyhmipvsw\n\nmub\nbmu\n\ncfhsjuqrin\njfinqcrbshu\nnwyjcuflmsrgxdeqhi\nfhiucqjrnso\n\nkdznqiyxu\nemzsrpac\ncszabv\n\nidvqsgemlu\nsdlmevqu\nqlmspeudv\n\nlo\nol\n\nzcdu\nbcqmdupjzr\nluazkdc\n\nwrju\nwuj\njauw\ncuwgj\njurw\n\nsypfjcxmkzh\nvrgpzcytm\npcdzmyo\nbrmwegzynqcp\nauzvcpqmy\n\nqdtxj\ndpjq\nqluajd\n\nbgvldaj\ncxb\nbxc\nrubxf\n\nziadtsbvuchlofeyjg\noajlevkdyirbtgqc\negldjvaiqtybkcor\nlitcrbeaojmdvgyx\n\nitcn\ncsjftqn\nntwc\ntncw\n\naqlmydwfn\nnwliqmfjca\nqwhlmnfa\nnfamqlw\n\nigxtv\ngvxt\n\ndlrkztoe\ngbcdeotf\ndyeoit\ngcdetxo\n\nyuso\nxaesw\nzfsy\nsu\n\nwq\nwq\nwq\nqxtw\nqw\n\nfynaektqvcbozhdxpuw\nwzirdbecylampuqs\n\nr\nwcpazh\npbnyxkt\nlgk\nevdsiuq\n\ntshrjadlymb\nhrftismyjald\nemwljutyarqsdgho\nstjxdhrlmapy\ntajsdhmlrpy\n\namb\njvadbm\nfms\nramjv\nhgtnqywzmkx\n\nskmwcopgnfhuab\nmpjvx\nmrtp\ntympq\nmp\n\nj\nvjtu\nscf\nt\n\navwomuriqfelzsdnhjybptkg\nyeltqzspmdkihnjvuwfgboar\n\nnhkequsi\nhuifnmwsqg\nmnosihugp\niuyhaslbjvntxr\npiuzshno\n\nrdguywbhevoaf\naojdgvhbrqyew\n\ndtcipqw\ndktic\ncetsfid\nidctm\ncitd\n\nxjnzvmlpi\nvduwgtsy\n\nxfybrjwtkndm\nguxtyvfabjn\njetfoqnxplyzi\n\nbrosek\nsoberk\nborsek\nskrboxdze\n\nuadtqoi\nitaqud\nudqtia\n\nijqrfe\nijqe\noqvj\n\nvernidqwmzpho\nwpvzhmoeid\nmviodhpzwe\n\nikultrxhjoz\nkihjlutzx\nxihkzlujt\ntuixjzhelk\njxhlutzik\n\nztnmevyjlbisdfupgkrwq\niaxytmouwljksngh\n\nya\ny\ny\ny\ny\n\ndfizts\nfvslqiwtc\nxdsgzitf\nsiptfmh\niskrnutyfe\n\nczlaqjrxoeivgpbsnkym\nbxwldzaikpemjfyosqtgvhcrn\nsqgaroynvkcbejiuzxlpm\n\nadtcenfvy\najmwvsdkf\n\nhgpyaov\nfqnaidbgruokxzhy\noawshgty\n\nvztswelufacbk\nadkfuvmo\n\nceqrx\nylgrm\nxr\nzihukwvrno\nrjfm\n\nc\nc\nc\nc\ncy\n\nnkzfxij\npevkoxydw\nuamtrbhlgcsq\n\nk\nvk\nxgk\nnk\nrk\n\nrokqypnfjm\nfvkrpjygnibqmo\nmpqykofjrn\nfjnpqkmoyr\njqoymprnfkw\n\nsgpv\npumasyv\n\neuhbkzytq\nkhqdzyeno\ngsfmzkhjeil\n\ng\ng\ng\n\nxnmucpeqvjizwhklbg\nhesokupzcjlrt\nuypfachlzejk\nekjuchltdfpz\n\npwaefhvgskymuql\npykgwvcsqomhfbnajzeu\nreqmsxkgpyfuwvaih\nagqumvpyfktshxdwe\n\nnpokjeqbiucfamxyrtw\nhaglxndtebiqojymwuzfr\niymxjtonrfbweqau\necwbxyarjmtoqnuif\n\nwmv\nqmof\n\nlvno\nrupsvno\nvfon\nvdno\n\nr\nrf\n\ncruektys\nckwuqeltbyr\ncektyur\n\nomwygfcpaliuqvteshzdxn\nvqbctzumoikefahdpnwglsx\nlitasvmxnoeudqgfphzwjc\ndesovqwhuimxzptngrflac\n\nzmyebqukpivjhorctsnldxag\nsnckjqrluyzwihaxmvpdoe\n\nawtsrxfhc\ntiqphvdywaz\nihwta\n\nrfqnegxpmjtzcihskvab\netmvrxifkqsgjnpbzcah\n\nnabdje\naezxjn\nbnezoa\nean\nqrenga\n\nrlhxpyo\nr\nmnorl\ntgwizrqvcbe\ndr\n\nvpqbjyseizrtn\nbljrtepdyqnvszi\nzbtrnysjpvqei\nbhgyqpvjenrtzis\n\nhkzcxmwug\nwygxkuzm\nwumxzgk\n\nrq\npq\nfu\nibzwnx\nhr\n\nduabykztcwnqxmflpse\nqxzdpbnfwmstcuakryl\nnlbctapxmkzuwdsyqf\ndyslfpuwbitqnzxahjckom\n\nqoacdtzusvixrh\neuzrowdtglmcikav\n\noutyzds\nbdlyztwso\nkbosyzwedgvjlt\n\na\na\na\na\na\n\npoejxws\njwoexsq\nexjsow\noexswj\n\niwqaesxpufgmr\nuxpwmfsaigq\nawmuxpisgqf\n\nycxhlrfpmnwtab\nrulhtdsf\n\njbgewroydxaiqfmkzv\nkdvyifqtazmocnws\nktvhwaoiydqmfz\nvmdokzqwayif\ntkfwaimdzyoqv\n\nsjqhftbamkzwyorlpve\nrsefbmpzhwvkajtqoyl\najpmbotsqhevlyrwkzf\ntmslfhqerjkvgxwayopnzb\ntpseohzrvqyjmwlkabf\n\nrtuekjwpcabm\nkrbupeminyha\n\nlcgnpwvkeuaxsqhimzorbydft\nhpjksmytlavfzcrundigqboexw\nvhpbzfqemyockrxgniadtuwls\nyowhvzpldfbtqgsxamcnkireu\nkvfbynscmrdaupegltzixwohq\n\ng\nigk\nmwfen\n\nbujfzi\nfbj\nrfjb\n\nkzxnlwcj\ncyezomtkwlj\ncujwlzak\nlawjczk\nckwsjlzx\n\nfwhyzdn\nqjhcdaew\n\nibrx\nuqosfw\nxiyre\ngixryl\n\nck\nkcs\nukca\ncks\n\nex\nw\nijmhbvkoe\nalhq\n\nat\nta\nuat\nga\n\ndcgitapjnokhsx\ncbrxumnqijlfvozsy\n\nwfvcx\nkop\nygensbhta\ncrqi\nmdz\n\ndwhcuniof\nihwmnfu\nohnudfwi\nkpnirgwjhuef\nihnwufq\n\nrlga\nsparg\n\nhytsabludcfv\njewgmrznp\n\nvlmqbx\npwrcexfhkzgyns\n\nw\nw\n\nav\nzvma\n\ncpjklr\ndyvns\na\n\nyn\nny\ngcyn\numznyhv\nyn\n\nwnvsafjgr\nwfngjiasrv\n\nievwnudhstf\nfsuvhtiew\nbheutiswvf\n\nudtnwkbheaf\nkucylwvjxiqt\n\nhljtmwnzcdkrfisv\nwdjhtlcnrskivz\nwjdvntscexqlripkbzhgay\n\nbkjmqivtuanscydxp\ncwqxidgpvbntusmakjy\ntbjkvyaixsdcpmqun\n\nftl\ne\ne\nu\n\nhkjcgexiqbs\nkbehocig\nhigkycoblet\nnblicogkeh\ngehikzbc\n\nwaingcjboqylezdsu\nnmyukizxdbwvoqgcaspf\ntyqdnbgcsoujiazrw\n\nlfchjito\nvlutojfcsh\n\njovtwiuheqkrdxapcsfgm\ncvawpxumglirokdtfqjhs\n\nkswydazphxc\nxwhsayzkncde\n\nlahity\nvfgipmabtyrn\nyhidtsa\nyati\naitqyh\n\nd\ng\n\nrphofvxebzy\nuzqfyd\n\nxyowpfhvtdkjersgalzi\nbgalejwmkrhycopfvtidsxz\njauyptlhwrvgszfoexkind\nrvkihpgyfawtlzdjxseo\n\nuqbnrhlasgdopwcvfemztkyjix\noafnpqgtdezrwlxkvmhcubsijy\n\nkgrovfpiatd\nsdrxfik\nhfibczyekmqrlu\n\noydl\ndohl\ndoyl\n\nbkvnawhqodjcrzpfxtl\nlmxcgkfbtqnwyphrouzjvia\ntwpvoxkhaqzsnjlrfbc\n\nno\nbnix\nnabv\ntdumsrc\na\n\nk\nqsj\naqjs\n\ntzfuo\nfzvxspjamchgio\nfozbq\nozf\n\natnxcrdus\nrinqbdasxf\ncaursxdnl\nzandsxr\n\nsaryuioetn\napemtnioyr\nalotvfiyngerq\nntpaoweruidy\n\nq\nit\nc\nq\n\nhzoiptgj\nwzpd\nhpzdj\nbzqkpv\n\nzsa\nza\n\nol\no\nh\n\nxf\nzndqlyf\nfk\n\nubaplvgsfqn\nvlbqefsgpn\nbqlsgnpfv\nlbpvqgnsf\n\nnphoablwjs\nhubyic\nzfmgibxeh\nhdbuyrg\n\nmtdi\ndhntuckim\ndtim\nsfmeidt\ndipxftm\n\nfxwnjhitovpde\nvokwtfjspmedib\ngqftcrelpovyzwid\ndwepovfitan\n\nnaphmuwlztbxsiqy\nbrpnyfqxto\ndqtrnpykbxe\nnpdbjtoyxq\ndvpqtbxyn\n\ntlhrgk\ntkjh\ncxbtov\ntjy\n\nukjgpahefm\neiajkdwmt\ndxawynmebikj\n\nozpmc\npmzco\npcmofz\nmcpojz\nzmovdcp\n\nszyamqourexh\nqesympcalfokh\n\nfmlotwur\nzdgwyiqmnk\n\ncyeqwlbki\ndnufjzstpvhr\n\nw\nw\nw\n\nefzqnjvhplgrmy\nbzmovalwqkyjgrxpf\nriydsvtqcufp\n\ntclrg\nrlycb\nedzkrlc\nclr\n\nljpfkhrzgmno\nzlhpkojfmrnxg\nhgprkymfnzljo\ndbhrgzklvnpioajtmf\n\nvybn\nvybn\nbynv\nvnyb\nvnyb\n\npvakljgcefuyxzwr\nuwcevpkflrxjgzay\n\nd\nd\nd\nd\nd\n\nsiy\nz\nc\n\nfpusnocme\nokum\nwmubojtyiazg\nlfxrkuqmdo\nxoum\n\nz\ni\ni\n\nexoimtjhqap\nxzbmulsdo\n\nnrkvlyhj\nrqhjnkyv\nnhkeyrvj\n\nfgdilbrjovqkxe\nriknjxmocevudl\nkoyixrdeljv\n\nfv\nvf\nfv\novf\nvf\n\ne\nqcdu\nyk\naiwrtg\n\naosmzekcfbg\ncogzvamfkestxb\nwucsqojzbfynrekgmap\nmszbokgefalc\ncbegakfsomz\n\nl\nlr\nl\nl\n\nipbrflxjntmyzwh\nxthbpywzonkidrfj\nvxtzirpjbnfwkyh\njbzfaiwtnuxyhgcspr\ntkqhyxbnfizpjwr\n\nidz\nngmuvotyjqlw\nfdpbz\nbi\nsfk\n\nu\nu\n\nxiwtjalesvy\nitsvdehxwy\nwzbkiovpnesmxgrt\n\nhqalfwptvmydruokzejincgbsx\nfuyjneqibopgvmdlratwhxkscz\n\nsoehbfdlunqayzixpcg\ndjfeosucbgplqh\nsvpwhkurtfgqbdoe\n\nsrjgvqtldz\nvzqrdgoljt\nlzsjqgdvrt\ntrlvdjzqg\n\nv\nh\np\np\n\ncjnm\nmcjn\npncjm\nmncj\nmjcn\n\niok\niok\niko\niko\n\nxcwgjue\nkgju\nazogjuft\njgmuxwc\n\nmsajntrlkxqeiohfv\ngetnrvjoflqxma\nhfqmjnvagxlotre\njyovtledxrnfmqab\nqxtverwjialonmf\n\nhksfi\nbeqrfzgoul\n\nczlmtbnrqjduah\nrdzgbnmhiauslct\ntcazsudprlhnbm\nnwctrlxbhvudzmfea\ntbcqnyzdrlmuha\n\ns\nm\nm\n\nqmxkiv\nsqcxm\n\nkvtbyfazojer\nrofevztkyajb\ntkazojvbfery\n\njrwld\nijward\njpyrdbwe\nrjwd\nrdwjm\n\niwartbsghcj\nitgwajlzs\n\nuoebjyairqxpn\nijauenbyxorq\narobqyxieunj\nruhiodexbyqja\njibecuxoryqa\n\nnk\nnm\nn\nmwn\nnw\n\nrkzdeiufl\nknibderzmufl\n\nophtrmdneysl\noelpndymtsrh\nptndroyehslm\ntrsendlympho\ntmpnhyseolrd\n\nvjdes\navps\nvs\n\nlrokj\nrlvgocjwmnhpxye\nljrof\njqfzlro\njrlo\n\najz\nmztjnk\ndojlgbzv\njzy\n\nnqg\nxgef\n\nqytgos\nqgt\n\nuixtyzmjskabndwl\nxzkujfpqrwtmdsioanb\ngdztwsjmkxieuan\nzwumkxqjdaivstn\n\nybekpljqhwaozrsunvc\nwabijepnshzvlmkucqoyr\n\nywq\nvwa\n\nozxvnabjchsurkyit\nhrcxntaiujfkbsvy\ncvytrqdikuahbsjxgn\nvkybjratchnfxius\n\nxkozfm\nfthk\nujyprq\n\ndmavqeft\nfacmbgevqtk\nqaftmev\n\nnqftsomeiwkh\nkodtfarsqmpnwzev\ngnwfskouxqmte\nkqymjenlstowf\ngqwmeckxfobstn\n\nw\nf\nf\nf\nf\n\nynouhdazs\nuhndzsqaoy\ndyarhuzsnwo\n\ntwvzmsuek\ntulsmekz\nsxmlidpuekz\nzmsenujko\n\npyid\npidy\nirsmdz\n\nnglvrjkmh\naltmnwkhfro\njrlzntym\nunqrimlexbsc\n\nrzmfbjnvtuhalspydkq\nkdctavibrufjzmpwhseql\n\nquj\nuq\nuirqdc\nuq\n\ncsorknptwzehgvx\nolzknwjieyhv\naebwnhvqkoz\n\nonqbzfcsykjrhpuelmdvwt\ngyunqefdoszkptcjlmhwb\naqbywdnxhjfuselkztopcm\npnlcosjfhymdwzvbutqeikr\n\nncs\ney\ndc\nypt\nqhvgowiuabkrx\n\no\no\no\no\n\nsi\ns\nsq\nezsaux\nrsf\n\nmxtindkegroflbps\nwopfiexsrnylgd\n\ncdpszukwehnqyotbarjg\ngohpwlyketrsza\nhyokzsrgeapxltw\n\nozbaxtrhsnjydpqfmlvckw\nykpqahmucslrzfwxodtv\n\ndro\ndolr\nidro\nodr\ndrojmps\n\nf\nf\nj\nf\nf\n\nx\ncxs\nxm\nxm\nyx\n\nlmjxn\ncdou\naqekouwci\n\nasqkcwirotd\niqctslwpkagro\nqwvfrjeixoctasdk\nksfvrtcaoweuqi\n\nrcxjokgweimzaqlsyvh\neiszkpamocvhux\n\nsvbneghqi\nwlngkm\n\nyx\nyx\nyx\nyx\n\nzgmtedunpharcvifyxsq\ncdwyhxupfamtgzqjnosrevlik\nrtshdumnvfxieypcgbqaz\nnepzuxfaqbtmyrdhgiscv\nyvcmtepxzqadfhrnuisg\n\nodvngjmuw\nnmjguvo\nipsjlgvboumnyh\nujvmgond\n\nibte\nlbj\n\nvnyazpoqm\nnyvouxaepms\nmpgyvjoacwn\ndymapvnkoh\n\nzvqmtfcbeikxwsgjudo\nzjodgqbamcwnevukftx\ntdwevmbqojukzfcxg\nqwykzudcbgmofexajvt\n\nhagrex\negrxah\n\nz\nz\n\ngmx\nwxtmg\n\nvh\nizshx\n\nvejrthqgixubla\nxwroqnjvzkptfbh\n\nngpauyltq\nqftxapncled\nqnpbwyualght\npytqnal\npwhqktlnua\n\nofelsiqtakyvgcdubhprm\narucvsxkezgifobqp\n\nsybhmruftp\nqmbyfrphsu\nqyfrphbusm\nfpusrhymb\n\nhaekngcs\ngahj\n\nycatzehdimog\nfvh\nhqk\nhq\n\nat\ni\nv\nv\nv\n\nfwsecblarvdo\nsdoravlewcfb\newjofrlavcdsbz\ndrfwcaoslvbe\n\nrwetumkldsgjfbqianohzv\nsvfkorqaduhlzigjtwben\nnfwougzjylprdqtvhsbaeki\n\nzdwskmcux\ntcpmxog\nfudcnzxm\n\nwmcleqrfozgstuhandipbykv\nwrfugkoblvazmsxpycjhtdqine\nokzimeadcrpwybvlghstqunf\n\ncroutqsxvyfg\nqelbwgca\ngcq\ngqcilm\ngqbpwcz\n\nvbjx\nxfeojybgw\njbxu\nujxb\n\nhumzypcl\nmlcuh\nmlchu\nmwclhu\nmcuhl\n\nslqntvhwxeby\nvhwxbltseyqn\nwtsxhvblneqy\n\nikby\ni\nit\ni\n\nloheyfgpaibvmkxn\nauivtdszjelhmcgxp\niykrlbagmevnhxp\n\nitnaqvyzbjluexgposwc\nxcoqgbusthpnvakzwlyie\nunqgycsvtwozxiblpea\ninzlgatuybvewqcxops\nxtlqgaezvcuyionwbsp\n\nsdbugmnzcwfqyvakjerx\nrebfqgcdmunxsyavwzjk\nszdwkavcbujngmryexfpq\nqevfkmrbxyungjzscadw\ngquzdwsykraxjnhelfvmcb\n\nzocknmwfbitjuv\ncbiojlnuzwt\nnyzbcwitjuo\nzocxjqntwiub\noajubtzrncwi\n\niys\nsyi\nwzispyhgb\ninsy\n\nudpvgixzhyobecns\nhugvxskpeoc\nsgpfoemxcujhvlw\ngxcqoeuvhsp\ncoavprushteqygx\n\nzsqtvr\nhqej\nsicqg\nhetqad\nqlkmpyxuowbfn\n\nlopqfgtzsbny\npuyfqbnlzsio\njnblqypsofzm\nsbyzolfqpn\npfqblykunsoz\n\niegjxynurlcftbzkph\ngynlhpdrfezukxcjbst\ncuzteyghlrxikfnbjp\nkzenfpyhrugblxtvcj\n\nhlupx\nfqpsux\nxsqup\npxuwf\n\ngzt\nzmgt\nngrmzlt\nzgtcs\n\nnsp\ns\nis\ns\n\nzsixudhgpcfblnqmve\nqxgpsczindufeblvmht\nxfnueqghlmsdbvcpiz\nqibfmhgselxcdznpvu\n\nfkxcogmz\ncxfmkogz\ngxfokmcz\nayhgxkoqfclmz\nocxfkgmz\n\nqxtd\nhxmwdgt\ncdtix\n\nvfyb\nepdk\np\nnpz\n\nbkvrjsygeczfpqomhuwdi\nwlrodcybkztqujmheapig\n\nkftsrqpxazb\nzfkxpqbs\n\nguysto\nysf\nsfy\nysh\nys\n\nsh\no\nvi\nz\nuh\n\nlbqrpxwsuvdec\nfselpdtqkvbrgu\n\nhmwjzxectfnsiqyl\nisxtqjywcnhfmze\n\nuefgopcqnktavdhljir\ndvceuhtlqnkpfi\nficqudvlhntkep\ninvtfpzhldcuemkq\n\ngjlhmdyun\nhlujdmgny\n\nzupwyo\nwyonzpu\nyozpuw\nyojwpzu\nywpuzoq\n\natculpzvwro\nvazwctrpuo\ntapwuzgvroc\n\nkwaxzer\nkwerxza\nwkrxeza\nakzerxw\nxwezark\n\npazsg\napwsz\n\ntshprib\nphisbr\npihrbs\nphbsir\nhrsibp\n\nishtmycqw\nthymsc\nsmtchy\nytscmh\n\nfwuhmsnvjclaipgbd\nhuogmlpcvdjwniazs\ngivwhcujkmsdnlzpax\n\nkfi\nf\nf\nf\n\nrotszqynjuwvdcmb\nutodajecnvhy\n\nxhmqfursgt\nuthagrnfxscm\nsqhtmfgwixrlyu\nlwrfxytpgushm\n\nl\nl\nl\nj\nl\n\njfgqrstv\nizaod\n\nrdzqnehxmwlpobuskvjiy\nuhkxpldyqnbwoszmiverj\nzrgpckwdusmixnebvloyfqhj\nuobekwirqxynzdmhvlspj\n\nqauvodcszfhleypxbgjrknt\nyvfdoahkxjnslpgqbrcuzet\nuqrdlzsnpyahtfboxcgvejk\nyjqelcdxrzusghfabktnpov\noetuxsfzyvlrkhgbnqcdpja\n\nlsgzdmvcykr\nzblvifkcpesjda\nqulndsmtcwzokv\n\ngyvnbcsaljqrfo\njhbfxlmqizayrswnu\nblsfnraqpyj\nafhylsrjnbq\n\ngaklz\nkaz\nzatnkc\n\nksgcejpyt\nvtisnmp\nontfspa\ntsp\n\ngpnklyvzdei\niylnwvzke\nziketylvun\nvznyklebi\nkvziajyqnhmcsel\n\nr\no\nr\no\nz\n\nug\nlug\nug\n\nydgtezlosuirxqhck\nzhgsqoeyxidctukrl\nritqhczsuekgdyxlo\nreluzxtksoycdhqig\ndcrziheolgsqkxuyt\n\npxgmjlwzycrtuonv\nxvrnlpyjzoutwgcm\nlxgtpyuzojcwnvrm\nncvrlpmyjgzwutxo\nlgcvojwxnuzmrpty\n\ntklvywfzjhx\nuzkftyhvlbxw\nykhwftxzvl\n\njukhnepb\nnbukephj\npuenjkhb\npunjherbk\n\nixsmkpvzqryd\nxifdkpoejy\n\nghldsu\norutphgf\nuhgc\n\npnkdgfcbmwlqjxh\nuwkgyhnsjblmqcd\n\ncui\niucqvj\nuxprfci\niwxcu\niurcx\n\nfomuvtrxj\ntxjumv\nuvtyjmx\nmujxtpkvd\nutfyemxvqhj\n\nkahpnit\nzdgtbcjehip\nguwjihbtplr\nmtxsivpqh\n\nhbmuerdji\npqjyxrlhnbi\n\nblup\nfupb\nbpu\npub\nubwmp\n\nntyhgwjekrxsudz\nbvdkzoprfutcjqgn\ngsaekrjtnmzud\ngnktzjxudr\nnzgtjkulidr\n\nkvamjswfgoul\nfmagujslv\njcugaslvfm\n\nbdtqeimuzpnhgavrw\nptmhzulsvqgnfrwkd\nkcyturwmqhzgvdpn\n\nlkvzyhnug\nzuglkyef\n\nxnsqbdhtiagwvrlpezyf\ndlntjkpzugsqwaxeryhf\nrpehgncayztmsqbfiwodlx\n\nfsbv\nryoptdf\n\nebxfakjriydp\nyijedfkprqba\npraekbjifdy\nibaejpfrkdy\n\nmyfvdnqsxiazjpg\njefvmxpaisdozlbcqn\nhkxqjdpnafrmsztv\n\nfaoknlebqpy\nkpfylnaoeqb\noankeqybflp\nnyeqkpfblao\nkobelfpaqyn\n\nb\nqierm\nby\n\nrdhgxubnkcot\nkbcouhmxtgd\nsmbtgcahuikyoxd\n\ncsuqvbzmopehfanytxlrwkdigj\nqrexnzoakupicfgthdsjybvmwl\nwpfkachnjtxgdysboilmuezrqv\n\nul\nlu\nlu\n\nqrb\nbycr\npwe\n\nsbfiuhzw\nmsouybvlzrhwdfj\nihvzblw\nxpbcqazwtekgn\n\nvouxdfiamelztswy\niflgzytmwsbduxe\niyfkjmspzwutxlde\nncldmpithsuyekwfbxgz\n\nijxcbyp\njbpiycx\nxybijpc\n\nwanuhtdyskljicxzebmfpvq\nlyguehpfsvcmtbkndwao\npkbdywhsvmealfctrun\n\nahbftkwivsczonj\nnwkzvahfbycjitos\njmznkbieoswtvafhc\nvztkiqjanhsbfwco\n\nolxzgkrjcandtwupehsqbify\nrzcaekovyijgxtwnbdsqhpul\n\nazhycqgnxjpudrvfko\noxfnhvkqpdcrzjgay\nhrebsqyjpaoglktdxznmcfv\nrjvzcyhdqpagfonxk\njygkqxfhrcvodznpa\n\npqkfrztwhgybncd\nyhgpobeuwatmkzqxnrd\ndzqgrvbyptkhnw\nsnyhilgrzwvqctkpdjb\n\nzcljnxvohbuqtyf\ncyqlfrhnxujobz\nbwxzhyoeusclfqnjmi\n\ngixeoy\nigoeyx\ngxoeyi\n\nxvfowdzkineas\ndpinwxejkfsavoz\nevkoxiznadfws\n\npfrxj\ndqearfgnt\n\nsfhkce\nnvbdk\nknv\n\noxpa\npoxa\nnxvah\npxa\nxam\n\nlafch\nhafnxlc\n\ndorcjz\naolei\n\nnszybcuheqmt\nszhjpvencm\nzhnedscmp\n\nwfdatxbsk\nucwklisqtfn\n\nvguconbkqjmw\njnqvmgkwu\nnqjwukgvm\njwvguqnmk\n\ngrajfdibqoknsu\nxecwhajtilpz\nyihpzjam\n\nkjzlpfao\neprntujdzhxwql\nvjlmcyazsp\n\neyvi\ntiye\niey\n\nmjlgsacntvyzqrdufhxpk\nftlzrhsjgvmnkupdxcqay\ndyvhtcruxmapsnljzqkfg\n\nshj\nlzsvj\nsjh\nmsj\njs\n\nzhoselpidtvnqac\ncqitodanpuxvzh\n\nih\ntzhe\nwhe\nmrdvh\n\nhtacu\nniatchu\nyucath\n\nadhflbns\nplhbdfas\nafshldb\nsdvhaiolqbgf\nksfdlhba\n\nfrsy\nfrsy\nsrfy\n\ndcq\ndkmeqcv\nqdc\ncdq\ncdyq\n\nawgp\nwp\nfpyw\n\nkjph\nxhkjp\n\nfjplvwnihqotkcsyd\ntdvusiypqkjcfowlh\n\nquyzmxeprlgona\nhwkseitjocfxr\ndzpqolenxr\nbrpeyox\n\nbnjhzetoldk\nftnizogykhamjbul\nzobtjesnklh\nqbkrtjlnohsz\nkbjpwoqznstlh\n\nw\nt\nwgq\nsanxjuzypkf\n\nwkpzeigcxqob\nwbgipzkqeoxc\nceqkgxbpizwo\nqbozgixckepw\n\nvibupghywqco\nvwiqhcgbpyou\nvycqgohubiwp\nobcguhvipqyw\npbujhvewogqyic\n\nqol\nlos\noghrfxa\n\nslorfjctxvw\njdfcyrtoxv\nxuozvcernibftj\nvcjfxwoytr\n\nawrehc\naicze\ncaeh\n\nqxvuhyp\nhxvcpy"
  println(sumCounts(input))
  println(sumCountsAll(input))
}
