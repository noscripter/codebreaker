var codebreaker = {
  settings: {},
  doUserGuess: function(guess) {
    var s = this.settings;
    // user makes guess
    var cows = 0;
    var bulls = 0;
    var error = "";
    var winner = "";
    // check for errors
    if (s.acceptableWords.indexOf(guess) === -1) {
      error = "<span class='red'>Not an acceptable word. Try again!</span>";
    } else {
      if (s.recordedGuesses.indexOf(guess) !== -1) {
        error = "<span class='red'>You can't try the same word twice! Try again.</span>";
      } else {
        // user guess is acceptable
        s.recordedGuesses.push(guess);
        if (guess === s.computerWord) {
          // user wins
          winner = "<span class='blue'>You win! My secret word was <span class='bold-caps'>"+s.computerWord+"</span>. My next guess was <span class='bold-caps'>"+this.getComputerGuess()[1]+".</span></span>";
          cows = 0;
          bulls = 4;
        } else {
          // determine score
          guess.split("").forEach(function(character,index){
            if (s.computerWord.indexOf(character) > -1) {
              if (s.computerWord[index] == character) { 
                bulls = bulls + 1;
              } else {
                cows = cows +1;
              }
            }
          });
        }
      }
    }
    return {
      "error": error,
      "winner": winner,
      "cows": cows,
      "bulls": bulls
    };
  },
  getComputerGuess: function() {
    var s = this.settings;
    // computer makes guess
    var scores = [];
    var computerGuessIndex;
    var computerGuess;
    
    if (s.recordedComputerGuesses.length > 0) {
      // rank remaining words
      s.remainingWords.forEach(function(remainingWord,remIndex){
        // each remaining word gets a score based on match to prev guesses
        var cowTotal = 0;
        var bullTotal = 0;
        s.recordedComputerGuesses.forEach(function(prevGuess){
          var prevWord = prevGuess[0];
          var prevCows = prevGuess[1];
          var prevBulls = prevGuess[2];
          var cowCount = 0;
          var bullCount = 0;
          remainingWord.split("").forEach(function(character,index){
            if (prevWord.indexOf(character) > -1) {
              if (prevWord[index] == character && prevBulls > 0) { 
                bullCount += (prevBulls >= bullCount+1) ? 1 : 0;
              } else if (prevCows > 0) {
                cowCount += (prevCows >= cowCount+1) ? 1 : 0;
              }
            }
          });
          cowTotal += cowCount;
          bullTotal += bullCount;
        });
        scores.push([remIndex,cowTotal+(bullTotal*2)]);
      });

      scores.sort(function(a,b){
        return b[1] - a[1];
      });

      // output computer's top five guesses
      // console.log(s.remainingWords[scores[0][0]]);
      // console.log(s.remainingWords[scores[1][0]]);
      // console.log(s.remainingWords[scores[2][0]]);
      // console.log(s.remainingWords[scores[3][0]]);
      // console.log(s.remainingWords[scores[4][0]]);

      computerGuessIndex = scores[0][0];
      computerGuess = s.remainingWords[computerGuessIndex];
    } else {
      computerGuessIndex = Math.floor(Math.random() * s.remainingWords.length) + 0;
      computerGuess = s.remainingWords[computerGuessIndex];
    }
    return [computerGuessIndex,computerGuess];
  },
  doComputerGuess: function() {
    var s = this.settings;
    var computerGuessIndex = this.getComputerGuess()[0];
    var computerGuess = this.getComputerGuess()[1];
    var computerCows = 0;
    var computerBulls = 0;
    var message = "";
    var winner = "";

    // evaluate computer guess
    if (computerGuess == s.userWord) {
      winner = "<span class='blue'>I win! My secret word was <span class='bold-caps'>"+s.computerWord+"</span>.</span>";
      computerCows = 0;
      computerBulls = 4;
    } else {
      // remove guessed word from index
      s.remainingWords.splice(computerGuessIndex,1);
      // test new guess
      computerGuess.split("").forEach(function(character,index){
        if (s.userWord.indexOf(character) > -1) {
          if (s.userWord[index] == character) { 
            computerBulls = computerBulls + 1;
          } else {
            computerCows = computerCows +1;
          }
        }
      });
      if (computerCows+computerBulls == 0) {
        // no match, so remove all words that have word's letters
        var newWords = [];
        s.remainingWords.forEach(function(word,index){
          var charCount = -1;
          word.split("").forEach(function(character){
            charCount += (computerGuess.indexOf(character) > -1) ? 1 : 0;
          });
          if (charCount == -1) newWords.push(word);
        });
        s.remainingWords = newWords;
      } else {
        // keep dictionary words that have x characters (guessScore) of computerGuess
        var guessScore = computerCows+computerBulls;
        var newWords = [];
        s.remainingWords.forEach(function(word,index){
          var wordScore = 0;
          word.split("").forEach(function(character){
            wordScore += (computerGuess.indexOf(character) > -1) ? 1 : 0;
          });
          if (wordScore >= guessScore) newWords.push(word);
        });
        s.remainingWords = newWords;
        s.recordedComputerGuesses.push([computerGuess,computerCows,computerBulls]);
      }
      message = "I've narrowed your word down to <span class='bold'>"+s.remainingWords.length+"</span> possibilities.";
    }
    s.round += 1;
    return {
      "computerGuess": computerGuess,
      "message": message,
      "winner": winner,
      "cows": computerCows,
      "bulls": computerBulls
    };       
  },
  init: function() {
    var self = this;
    var s = this.settings;
    s.acceptableWords = ["abed","abet","able","ably","abos","abri","abut","abye","abys","aced","aces","ache","achy","acid","acme","acne","acre","acts","acyl","adit","ados","adze","aeon","aero","aery","aged","ager","ages","agin","agio","agly","agon","ague","ahed","ahem","ahis","ahoy","aide","aids","ails","aims","ains","airn","airs","airt","airy","aits","akin","albs","alec","alef","ales","alif","alit","alky","alme","alms","aloe","alow","alps","also","alto","alts","alum","ambo","amen","amid","amie","amin","amir","amis","amok","amps","amus","amyl","ands","anes","anew","anil","anis","ankh","ante","anti","ants","anus","aped","aper","apes","apex","apod","apos","apse","arbs","arch","arco","arcs","ares","arfs","arid","aril","arks","arms","army","arse","arts","arty","arum","arvo","aryl","asci","ashy","ates","atom","atop","auks","auld","aunt","auto","aver","aves","avid","avos","avow","awed","awes","awls","awns","awny","awol","awry","axed","axel","axes","axil","axis","axle","axon","ayes","ayin","azon","bach","back","bade","bads","bags","baht","bail","bait","bake","bald","bale","balk","balm","bals","bams","band","bane","bang","bani","bank","bans","baps","bard","bare","barf","bark","barm","barn","bars","base","bash","bask","bast","bate","bath","bats","baud","bawd","bawl","bays","bead","beak","beam","bean","bear","beat","beau","beck","beds","bedu","begs","bels","belt","bema","bend","bens","bent","berg","berk","berm","best","beta","beth","bets","bevy","beys","bhut","bias","bice","bide","bids","bier","bigs","bike","bile","bilk","bima","bind","bine","bins","bint","biog","bios","bird","birk","birl","biro","bise","bisk","bite","bits","bize","blae","blah","blam","blat","blaw","bled","blet","blew","blin","blip","bloc","blog","blot","blow","blue","blur","boar","boas","boat","bock","bode","bods","body","bogs","bogy","boil","bola","bold","bole","bolt","bond","bone","bong","bonk","bony","bops","bora","bore","bork","born","bort","bosh","bosk","bota","both","bots","bout","bowl","bows","boxy","boys","brad","brae","brag","bran","bras","brat","braw","bray","bred","bren","brew","brie","brig","brim","brin","brio","bris","brit","bros","brow","brut","brux","buck","buds","bugs","buhl","buhr","bulk","bumf","bump","bums","buna","bund","bung","bunk","buns","bunt","buoy","bura","burd","burg","burl","burn","burp","burs","bury","bush","busk","bust","busy","bute","buts","buys","byes","byre","byrl","byte","cabs","cade","cadi","cads","cafe","cage","cagy","caid","cain","cake","caky","calf","calk","calm","calo","calx","came","camo","camp","cams","cane","cans","cant","cape","caph","capo","caps","carb","card","care","cark","carl","carn","carp","cars","cart","case","cash","cask","cast","cate","cats","caul","cave","cavy","caws","cays","cedi","ceil","cels","celt","cent","ceps","cero","chad","chai","cham","chao","chap","char","chat","chaw","chay","chef","chew","chez","chia","chid","chin","chip","chis","chit","chon","chop","chow","chub","chug","chum","ciao","cigs","cine","cion","cire","cist","cite","city","clad","clag","clam","clan","clap","claw","clay","clef","clew","clip","clod","clog","clon","clop","clot","cloy","club","clue","coal","coat","coax","cobs","coda","code","cods","coed","coft","cogs","coif","coil","coin","coir","coke","coky","cola","cold","cole","cols","colt","coly","coma","comb","come","comp","cone","coni","conk","cons","cony","cope","cops","copy","cord","core","corf","cork","corm","corn","cors","cory","cosh","cost","cosy","cote","cots","coup","cove","cowl","cows","cowy","coxa","coys","cozy","crab","crag","cram","crap","craw","cred","crew","crib","cris","crit","crop","crow","crud","crus","crux","cube","cubs","cuds","cued","cues","cuif","cuke","culm","cult","cunt","cups","curb","curd","cure","curf","curl","curn","curs","curt","cusk","cusp","cute","cuts","cwms","cyan","cyma","cyme","cyst","czar","dabs","dace","daft","dago","dags","dahl","dahs","dais","daks","dale","dals","dame","damn","damp","dams","dang","dank","dans","daps","darb","dare","dark","darn","dart","dash","date","dato","daub","daut","davy","dawk","dawn","daws","dawt","days","daze","deaf","deal","dean","dear","debs","debt","deck","deco","defi","deft","defy","deil","delf","deli","dels","delt","demo","demy","deni","dens","dent","deny","derm","desk","deva","devs","dews","dewy","dexy","deys","dhak","dhal","dhow","dial","dibs","dice","dick","diel","dies","diet","difs","digs","dike","dime","dims","dine","ding","dink","dino","dins","dint","diol","dips","dipt","dire","dirk","dirl","dirt","disc","dish","disk","dita","dite","dits","ditz","diva","dive","djin","doat","doby","dock","docs","doer","does","doge","dogs","dogy","doit","dole","dols","dolt","dome","doms","dona","done","dong","dons","dopa","dope","dopy","dore","dork","dorm","dorp","dors","dory","dose","dost","dote","doth","dots","doty","doum","dour","doux","dove","down","dows","doxy","doze","dozy","drab","drag","dram","drat","draw","dray","dreg","drek","drew","drib","drip","drop","drub","drug","drum","drys","dual","dubs","duce","duci","duck","duct","duel","dues","duet","dugs","duit","duke","duly","duma","dumb","dump","dune","dung","dunk","duns","dunt","duos","dupe","dups","dura","dure","durn","duro","dusk","dust","duty","dyer","dyes","dyke","dyne","each","earl","earn","ears","east","easy","eath","eats","eaux","ebon","echo","echt","ecru","ecus","edgy","edhs","edit","efts","egad","egal","egis","egos","elan","elds","elhi","elks","elms","elmy","emic","emir","emit","emus","emyd","ends","engs","enol","enow","enuf","envy","eons","epha","epic","epos","eras","ergo","ergs","erns","eros","erst","espy","etas","etch","eths","etic","etna","etui","euro","evil","exam","exit","exon","expo","eyas","eyra","fabs","face","fact","fade","fado","fads","fags","fail","fain","fair","fake","falx","fame","fane","fang","fano","fans","fard","fare","farl","farm","faro","fart","fash","fast","fate","fats","faun","faux","fave","fawn","fays","faze","feal","fear","feat","feck","feds","fehs","felt","fems","fend","fens","feod","fern","fest","feta","fets","feud","feus","fiar","fiat","fibs","fice","fico","fido","fids","figs","fila","file","film","filo","fils","find","fine","fink","fino","fins","fire","firm","firn","firs","fisc","fish","fist","fits","five","fixt","flab","flag","flak","flam","flan","flap","flat","flaw","flax","flay","flea","fled","flew","flex","fley","flic","flip","flir","flit","floc","floe","flog","flop","flow","flub","flue","flus","flux","foal","foam","fobs","foci","foes","fogs","fogy","fohn","foil","foin","fold","folk","fond","fons","font","fops","fora","forb","ford","fore","fork","form","fort","foul","four","fowl","foxy","foys","fozy","frae","frag","frap","frat","fray","fret","frig","frit","friz","froe","frog","from","frow","frug","fubs","fuci","fuck","fuds","fuel","fugs","fuji","fume","fumy","fund","funk","funs","furl","furs","fury","fuse","futz","fuze","fyce","fyke","gabs","gaby","gadi","gads","gaed","gaen","gaes","gain","gait","gale","gals","gamb","game","gamp","gams","gamy","gane","gaol","gape","gaps","gapy","garb","gars","gash","gasp","gast","gate","gats","gaud","gaum","gaun","gaur","gave","gawk","gawp","gays","gaze","gear","geck","geds","geld","gels","gelt","gems","gens","gent","genu","germ","gest","geta","gets","geum","ghat","ghis","gibe","gibs","gids","gied","gien","gies","gift","gild","gilt","gimp","gink","gins","gips","gird","girl","girn","giro","girt","gist","gite","gits","give","glad","glam","gled","glen","gley","glia","glib","glim","glob","glom","glop","glow","glue","glum","glut","gnar","gnat","gnaw","gnus","goad","goal","goas","goat","gobs","goby","gods","goer","goes","gold","golf","gone","gore","gorm","gorp","gory","gosh","goth","gout","gowd","gowk","gown","goys","grab","grad","gram","gran","grat","gray","grew","grey","grid","grim","grin","grip","grit","grok","grot","grow","grub","grue","grum","guan","guar","guck","gude","guid","gulf","gulp","guls","gums","gunk","guns","gush","gust","guts","guvs","guys","gybe","gyms","gyps","gyre","gyri","gyro","gyve","habu","hack","hade","hadj","haed","haem","haen","haes","haet","haft","hags","haik","hail","hair","haji","hake","haku","hale","half","halm","halo","halt","hame","hams","hand","hang","hank","hant","haps","hard","hare","hark","harl","harm","harp","hart","hasp","hast","hate","hats","haul","haut","have","hawk","haws","hays","haze","hazy","head","heal","heap","hear","heat","heck","heft","heil","heir","held","helm","helo","help","hemp","hems","hens","hent","herb","herd","herl","herm","hern","hero","hers","hest","hets","hewn","hews","hick","hide","hied","hies","hike","hila","hilt","hims","hind","hins","hint","hips","hire","hisn","hist","hits","hive","hoar","hoax","hobs","hock","hods","hoed","hoer","hoes","hogs","hoke","hold","hole","holk","holm","holp","hols","holt","holy","home","homy","hone","hong","honk","hons","hope","hops","hora","horn","hose","host","hots","hour","hove","howe","howf","howk","howl","hows","hoya","hoys","hubs","huck","hued","hues","huge","hugs","huic","hula","hulk","hump","hums","hung","hunk","huns","hunt","hurl","hurt","husk","huts","hwan","hyla","hymn","hype","hypo","hyps","hyte","iamb","ibex","iced","ices","ichs","icky","icon","idea","idem","ides","idle","idly","idol","idyl","iglu","ikat","ikon","ilea","ilex","ilka","ilks","imps","inby","inch","info","inks","inky","inly","inro","into","ions","iota","ired","ires","irks","iron","isba","isle","itch","item","izar","jabs","jack","jade","jags","jail","jake","jamb","jams","jane","jape","jarl","jars","jato","jauk","jaup","jaws","jays","jean","jehu","jeon","jerk","jest","jets","jeux","jews","jiao","jibe","jibs","jigs","jilt","jimp","jink","jins","jinx","jism","jive","jivy","jobs","jock","joes","joey","jogs","john","join","joke","joky","jole","jolt","josh","jota","jots","jouk","jowl","jows","joys","juba","jube","juco","judo","juga","jugs","juke","jump","junk","jupe","jura","jury","just","jute","juts","kabs","kadi","kaes","kafs","kagu","kaif","kail","kain","kale","kame","kami","kane","kaon","kaph","karn","kart","kats","kayo","kays","kbar","keas","kefs","kegs","keir","kelp","kelt","kemp","keno","kens","kent","kepi","keps","kept","kerb","kerf","kern","keto","keys","khaf","khan","khat","khet","khis","kibe","kids","kief","kier","kifs","kiln","kilo","kilt","kina","kind","kine","king","kino","kins","kips","kirn","kirs","kist","kite","kith","kits","kiva","knap","knar","knew","knit","knob","knop","knot","know","knur","koan","koas","kobs","koel","kohl","kois","koji","kola","koph","kops","kora","kore","kors","kris","kudo","kues","kufi","kuna","kune","kvas","kyar","kyat","kyes","kyte","labs","lace","lack","lacs","lacy","lade","lads","lady","lags","laic","laid","lain","lair","lake","lakh","laky","lamb","lame","lamp","lams","land","lane","lang","lank","laps","lard","lari","lark","lars","lase","lash","last","late","lath","lati","lats","latu","laud","lave","lavs","lawn","laws","lays","laze","lazy","lead","leaf","leak","lean","leap","lear","leas","lech","left","legs","lehr","leis","leks","leku","lend","leno","lens","lent","lept","lest","lets","leud","leva","levo","levy","lewd","leys","liar","libs","lice","lich","lick","lido","lids","lied","lief","lien","lier","lies","lieu","life","lift","like","lima","limb","lime","limn","limo","limp","limy","line","ling","link","lino","lins","lint","liny","lion","lipa","lipe","lips","lira","lire","lisp","list","lite","lits","litu","live","load","loaf","loam","loan","lobe","lobs","loca","loch","loci","lock","lode","loft","loge","logs","logy","loid","loin","lone","long","lope","lops","lord","lore","lorn","lory","lose","lost","lota","loth","loti","lots","loud","loup","lour","lout","love","lowe","lown","lows","lube","luce","luck","lude","lues","luge","lugs","luma","lump","lums","luna","lune","lung","lunk","lunt","luny","lure","lurk","lush","lust","lute","lutz","luvs","luxe","lwei","lych","lyes","lynx","lyre","lyse","mabe","mace","mach","mack","macs","made","mads","maes","mage","magi","mags","maid","mail","main","mair","make","mako","male","malt","mane","mano","mans","many","maps","marc","mare","mark","marl","mars","mart","mash","mask","mast","mate","math","mats","maud","maul","maun","maut","mawn","maws","maxi","mayo","mays","maze","mazy","mead","meal","mean","meat","meds","mega","megs","meld","mels","melt","mend","meno","menu","meou","meow","merc","merk","merl","mesa","mesh","meta","meth","mewl","mews","mhos","mibs","mica","mice","mick","mics","mids","mien","migs","mike","mild","mile","milk","milo","mils","milt","mina","mind","mine","mink","mint","minx","mips","mire","mirk","mirs","miry","mise","miso","mist","mite","mity","mixt","moan","moas","moat","mobs","mock","mocs","mode","modi","mods","mogs","moil","moke","mola","mold","mole","mols","molt","moly","monk","mons","mony","mope","mops","mopy","mora","more","morn","mors","mort","mosh","mosk","most","mote","moth","mots","moue","move","mown","mows","moxa","much","muck","muds","mugs","mule","muni","muns","muon","mura","mure","murk","muse","mush","musk","must","mute","muts","mycs","myna","myth","nabe","nabs","nags","naif","nail","name","naoi","naos","nape","naps","narc","nard","nark","nary","nave","navy","nays","nazi","neap","near","neat","nebs","neck","negs","neif","nema","nerd","nest","nets","neuk","neum","nevi","news","newt","next","nibs","nice","nick","nide","nigh","nils","nims","nipa","nips","nite","nits","nixe","nixy","nobs","nock","node","nodi","nods","noel","noes","nogs","noil","noir","noma","nome","noms","nope","nori","norm","nose","nosh","nosy","nota","note","nous","nova","nows","nowt","nubs","nude","nuke","numb","nurd","nurl","nuts","oafs","oaks","oaky","oars","oast","oath","oats","obas","obes","obey","obia","obis","obit","ocas","odah","odas","odea","odes","odic","odyl","ofay","ogam","ogle","ogre","ohed","ohia","ohms","oils","oily","oink","okas","okay","okeh","okes","okra","olds","oldy","olea","oles","omen","omer","omit","once","ones","only","onus","onyx","opah","opal","oped","open","opes","opts","opus","orad","oral","orbs","orby","orca","orcs","ores","orgy","orle","orts","oryx","osar","otic","ouch","ouds","ouph","ours","oust","outs","oval","oven","over","ovum","owed","owes","owls","owns","owse","oxen","oxes","oxid","oxim","oyer","oyes","oyez","pace","pack","pacs","pact","pacy","padi","pads","page","paid","paik","pail","pain","pair","pale","palm","pals","paly","pams","pane","pang","pans","pant","pard","pare","park","pars","part","pase","pash","past","pate","path","pats","paty","pave","pawl","pawn","paws","pays","peag","peak","peal","pean","pear","peas","peat","pech","peck","pecs","peds","pegs","pehs","pein","pelf","pelt","pend","pens","pent","peon","peri","perk","perm","pert","perv","peso","pest","pets","pews","pfui","phat","phew","phis","phiz","phon","phot","phut","pial","pian","pias","pica","pice","pick","pics","pied","pier","pies","pigs","pika","pike","pile","pily","pima","pina","pine","ping","pink","pins","pint","piny","pion","pirn","pish","piso","pita","pith","pits","pity","pixy","plan","plat","play","plea","pleb","pled","plew","plex","plie","plod","plot","plow","ploy","plug","plum","plus","pock","pods","poem","poet","pogy","pois","poke","poky","pole","pols","poly","pome","poms","pond","pone","pong","pons","pony","pore","pork","porn","port","pose","posh","post","posy","pots","pouf","pour","pout","pows","poxy","pram","prao","prat","prau","pray","prex","prey","prez","prig","prim","proa","prod","prof","prog","prom","pros","prow","ptui","pubs","puce","puck","puds","pugh","pugs","puja","puke","pula","pule","puli","puls","puma","puna","pung","punk","puns","punt","puny","pure","puri","purl","purs","push","puts","putz","pyas","pyes","pyic","pyin","pyre","pyro","qadi","qaid","qats","qoph","quad","quag","quai","quay","quey","quid","quin","quip","quit","quiz","quod","race","rack","racy","rads","raft","rage","ragi","rags","raid","rail","rain","rais","rake","raki","raku","rale","rami","ramp","rams","rand","rang","rani","rank","rant","rape","raps","rapt","rase","rash","rasp","rate","rath","rato","rats","rave","raws","rays","raze","read","real","ream","reap","rebs","reck","recs","redo","reds","refs","reft","regs","reif","rein","reis","rely","rems","rend","rent","repo","reps","resh","rest","rets","revs","rhea","rhos","rhus","rial","rias","ribs","rice","rich","rick","ride","rids","riel","rife","rifs","rift","rigs","rile","rime","rims","rimy","rind","ring","rink","rins","riot","ripe","rips","rise","risk","rite","ritz","rive","road","roam","roan","robe","robs","rock","rocs","rode","rods","roes","roil","role","rolf","romp","roms","rope","ropy","rose","rosy","rota","rote","roti","rotl","rots","roue","roup","rout","roux","rove","rows","rube","rubs","ruby","ruck","rude","rued","rues","ruga","rugs","ruin","rule","ruly","rump","rums","rune","rung","runs","runt","ruse","rush","rusk","rust","ruth","ruts","ryas","ryes","ryke","rynd","ryot","sabe","sack","sade","sadi","safe","sage","sago","sagy","said","sail","sain","sake","saki","sale","salp","salt","same","samp","sand","sane","sang","sank","sard","sari","sark","sate","sati","saul","save","sawn","scab","scad","scag","scam","scan","scar","scat","scop","scot","scow","scry","scud","scum","scup","scut","seal","seam","sear","seat","sect","sego","seif","self","semi","send","sent","sept","sera","serf","seta","sewn","sext","sexy","shad","shag","sham","shat","shaw","shay","shea","shed","shew","shim","shin","ship","shit","shiv","shmo","shod","shoe","shog","shop","shot","show","shri","shul","shun","shut","shwa","sial","sice","sick","side","sidh","sift","sigh","sign","sika","sike","sild","silk","silo","silt","sima","simp","sine","sing","sinh","sink","sipe","sire","site","sith","size","sizy","skag","skat","skeg","skep","skew","skid","skim","skin","skip","skit","skua","slab","slag","slam","slap","slat","slaw","slay","sled","slew","slid","slim","slip","slit","slob","sloe","slog","slop","slot","slow","slub","slue","slug","slum","slur","slut","smew","smit","smog","smug","smut","snag","snap","snaw","sned","snib","snip","snit","snob","snog","snot","snow","snub","snug","snye","soak","soap","soar","soba","soca","sock","soda","sofa","soft","soil","soja","soke","sola","sold","sole","soli","soma","some","sone","song","soph","sora","sorb","sord","sore","sori","sorn","sort","soth","souk","soul","soup","sour","sown","soya","spae","spam","span","spar","spat","spay","spaz","spec","sped","spew","spic","spik","spin","spit","spiv","spot","spry","spud","spue","spun","spur","stab","stag","star","staw","stay","stem","step","stew","stey","stir","stoa","stob","stop","stow","stub","stud","stum","stun","stye","suba","such","suck","sued","suer","suet","sugh","suit","sulk","sumo","sump","sung","sunk","supe","sura","surd","sure","surf","swab","swag","swam","swan","swap","swat","sway","swig","swim","swob","swop","swot","swum","sybo","syce","syke","syli","sync","syne","syph","tabs","tabu","tace","tach","tack","taco","tads","tael","tags","tahr","tail","tain","take","talc","tale","tali","talk","tame","tamp","tams","tang","tank","tans","taos","tape","taps","tare","tarn","taro","tarp","tars","task","taus","tavs","taws","taxi","teak","teal","team","tear","teas","tech","teds","tegs","tela","tels","temp","tend","tens","tepa","term","tern","tews","thae","than","thaw","them","then","thew","they","thin","thio","thir","this","thou","thro","thru","thud","thug","thus","tick","tics","tide","tidy","tied","tier","ties","tike","tile","tils","time","tine","ting","tins","tiny","tips","tire","tirl","tiro","tivy","toad","toby","tods","tody","toea","toed","toes","tofu","toga","togs","toil","toke","tola","told","tole","tolu","tomb","tome","toms","tone","tong","tons","tony","tope","toph","topi","tops","tora","torc","tore","tori","torn","tors","tory","tosh","tour","town","tows","towy","toys","trad","tram","trap","tray","tref","trek","tres","trey","trig","trim","trio","trip","trod","trog","trop","trow","troy","true","trug","tsar","tuba","tube","tubs","tuck","tufa","tugs","tuis","tule","tump","tuna","tune","tung","tuns","tups","turd","turf","turk","turn","tush","tusk","twae","twas","twig","twin","twos","tyer","tyes","tyin","tyke","tyne","type","typo","tyre","tyro","tzar","udon","udos","ughs","ugly","ukes","ulan","ulna","ulva","umbo","umps","unai","unbe","unci","unco","unde","undo","undy","unit","unto","upas","upby","updo","upon","urbs","urds","urea","urge","uric","urns","urps","ursa","used","user","utas","utes","uvea","vacs","vagi","vail","vain","vair","vale","vamp","vane","vang","vans","vars","vary","vase","vast","vats","vatu","vaus","vaws","veal","veil","vein","vela","veld","vena","vend","vent","vera","verb","vert","very","vest","veto","vets","vext","vial","vibe","vice","vide","vids","vied","vier","vies","view","viga","vigs","vile","vims","vina","vine","vino","viny","viol","virl","visa","vise","vita","voes","void","vole","volt","vote","vows","vrow","vugh","vugs","wabs","wack","wade","wadi","wads","wady","waes","waft","wage","wags","waif","wail","wain","wair","wait","wake","wale","walk","waly","wame","wand","wane","wank","wans","want","wany","waps","ward","ware","wark","warm","warn","warp","wars","wart","wary","wash","wasp","wast","wats","wauk","waul","waur","wave","wavy","waxy","ways","weak","weal","wean","wear","webs","weds","weft","weir","weka","weld","welt","wend","wens","went","wept","wert","west","wets","wham","whap","what","when","whet","whey","whid","whig","whim","whin","whip","whir","whit","whiz","whoa","whom","whop","whup","whys","wich","wick","wide","wife","wigs","wild","wile","wilt","wily","wimp","wind","wine","wing","wink","wino","wins","winy","wipe","wire","wiry","wise","wish","wisp","wist","wite","with","wits","wive","woad","woes","wogs","woke","woks","wold","wolf","womb","wonk","wons","wont","wops","word","wore","work","worm","worn","wort","wost","wots","wove","wrap","wren","writ","wych","wyes","wyle","wynd","wyns","wyte","xyst","yack","yagi","yags","yaks","yald","yams","yang","yank","yaps","yard","yare","yarn","yaud","yaup","yawl","yawn","yawp","yaws","yeah","yean","year","yeas","yech","yeld","yelk","yelp","yens","yeps","yerk","yeti","yeuk","yews","yids","yins","yipe","yips","yird","ylem","yobs","yock","yodh","yods","yoga","yogh","yogi","yoke","yoks","yolk","yond","yoni","yore","your","yous","yowe","yowl","yows","yuan","yuca","yuch","yuck","yuga","yuks","yule","yups","yurt","yutz","ywis","zags","zany","zaps","zarf","zeal","zebu","zeds","zein","zeks","zeps","zerk","zero","zest","zeta","zigs","zinc","zine","zing","zins","zips","zits","zoea","zoic","zona","zone","zonk","zori","zouk","zyme"],
    s.remainingWords = s.acceptableWords.slice();
    s.computerWord = s.acceptableWords[Math.floor(Math.random() * s.acceptableWords.length) + 0].toLowerCase();
    s.userWord = "";
    s.recordedGuesses = [];
    s.recordedComputerGuesses = [];
    s.round = 0,
    s.$guessContainer = $("#guess-container");
    s.$scorecard = $("#guess-container").html();

    // setup page
    $("input").focus();
    var startoff = window.innerHeight-150;
    $("#game-container").css("min-height",startoff+"px");
    $(window).on("scroll",function(){
      var scrollTop = $(window).scrollTop();
      var elementOffset = $('.footer').offset().top;
      var distance = (elementOffset - scrollTop);
      var pctoff = distance/startoff;
      $("#about-float").css("opacity",pctoff)
    });
    var wordBankString = "";
    s.acceptableWords.forEach(function(word,i){
      var comma = (i < s.acceptableWords.length-1) ? ", " : "";
      wordBankString += word+comma;
    });
    $("#wordbank").html(wordBankString);

    // record user secret word
    $("#user-word").on("keyup",function(event){
      s = self.settings;
      if (event.which == 13) {
        event.preventDefault();
        s.userWord = $(this).val().toLowerCase();
        $(this).val("");
        if (s.acceptableWords.indexOf(s.userWord) === -1) {
          $("#message-line-2").html("<span class='red'>That's not an acceptable word. Try again!</span>");
        } else {
          $("#input-user-word").hide();
          $("#input-guess").show();
          $("#message-line-1").html("<span class='bold-caps'>"+s.userWord+"</span> is your secret word. I've picked a word, too! Try to guess mine and I'll try to guess yours.");
          $("#message-line-2").html("");
          $("input").focus();
        }
      }
    });

    // user guess
    $("#guess").on("keyup",function(event){
      s = self.settings;
      if (event.which == 13) {
        // send guess for evaluation
        $("input").attr("disabled", "disabled");
        var guess = $(this).val().toLowerCase();
        $(this).val("");
        var userObj = self.doUserGuess(guess);
        
        // handle guess results
        if (userObj.error.length > 0) {
          // error
          $("#message-line-2").html(userObj.error);
          $("input").removeAttr("disabled").focus();
        } else {
        	// no user error
          if (s.round > 0) s.$guessContainer.prepend(s.$scorecard);
          $("#message-line-1").html("<span class='bold-caps'>"+s.userWord+"</span> is your secret word.");
          $(".round-num").eq(0).html(s.round+1);
          $(".user-guess").eq(0).html(guess);
          $(".user-guess-x").eq(0).html('<div style="padding-left:3px">'+userObj.bulls+'</div>');
          $(".user-guess-o").eq(0).html('<div style="padding-left:3px">'+userObj.cows+'</div>');

	        if (userObj.winner.length > 0) {
	          // user won
	          $("#message-line-2").html(userObj.winner);
	          $("input").hide();
	        } else {
	          // computer's turn
	          var timeoutID = window.setTimeout(function(){
	            var computerObj = self.doComputerGuess();
	            $(".computer-guess").eq(0).html(computerObj.computerGuess);
	            if (computerObj.winner.length > 0) {
	              // computer won
	              $("#message-line-2").html(computerObj.winner);
	              $("input").hide();
	            } else {
	              // continue
	              $("#message-line-2").html(computerObj.message);
	              $("input").removeAttr("disabled").focus();
	            }
	            $(".computer-guess-x").eq(0).html('<div style="padding-left:3px">'+computerObj.bulls+'</div>');
	            $(".computer-guess-o").eq(0).html('<div style="padding-left:3px">'+computerObj.cows+'</div>');
	            clearTimeout(timeoutID);
	          }, 500);
	        }
        }
      }
      event.preventDefault();
    });
  }
};