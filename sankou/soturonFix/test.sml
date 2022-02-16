type jumanOutputSemTy = 
  {
   代表表記 : string option,
   カテゴリ : string option,
   漢字読み : string option,
   (*
   ドメイン : string option,
   固有名詞 : string option,
   *)
   (*
   見出し語間の意味関係
   *)
   反義 : string option
   (*
   読みの音訓情報
   *)
  }

type jumanOutputTy = 
  {1_表層形 : string, 
   2_読み : string,
   3_見出し語 : string,
   4_品詞大分類 : string,
   5_品詞大分類_ID : int,
   6_品詞細分類 : string,
   7_品詞細分類_ID : int,  
   8_活用型 : string option,
   9_活用型_ID : int option,
   10_活用形 : string option,
   11_活用形_ID : int option,
   12_意味情報 : jumanOutputSemTy option
   }

(* 象 ぞう 象 名詞 6 普通名詞 1 * 0 * 0 "代表表記:象/ぞう カテゴリ:動物 漢字読み:音" *)
(* "の間の文字列を取り出す　*)
(* @があれば *)

   val outputExsample : jumanOutputTy = 
    {1_表層形 = "象", 
     2_読み = "ぞう",
     3_見出し語 = "象",
     4_品詞大分類 = "名詞", (*datatype hinsi = 名詞 | 形容詞..*)
     5_品詞大分類_ID = 6,   (*なくていい?*)
     6_品詞細分類 = "普通名詞",
     7_品詞細分類_ID = 1,
     8_活用型 = NONE,        (*オプション使ってもよい*)
     9_活用型_ID = NONE,
     10_活用形 = NONE,
     11_活用形_ID = NONE,
     12_意味情報 = 
        SOME
        {
         代表表記 = SOME "長い/ながい",
         カテゴリ = NONE,
         漢字読み = NONE,
         反義 = SOME "形容詞:短い/みじかい"
        }
     }


fun split moji = 
  let
    fun p s = print (Substring.string s ^ "\n")
    fun strTosubstr str = Substring.full str
    val substr = strTosubstr moji
    val _ = p substr
    val leftCut = Substring.splitl (fn c => not (c = #"\"")) substr
    fun rightCuty substr = 
      let  
        exception NIL
      in
        (if Substring.string substr = "" then raise NIL else Substring.splitr (fn c => not (c = #"\"")) substr)
        handle NIL => (Substring.full "NNILL", Substring.full "")
      end
    val rightCut = rightCuty (#2 leftCut)
    val sub1 = #1 rightCut
    val sub2 = Substring.slice (sub1, 1, SOME (Substring.size sub1 - 2))
    val result = Substring.string sub2
  in
    result
  end


fun Juman str = 
  let
    val tmpName = OS.FileSys.tmpName () 
    val cmd = "echo " ^ str ^ "| jumanpp > " ^ tmpName 
    val _ = OS.Process.system cmd
    val ins = TextIO.openIn tmpName
    val contents = TextIO.inputAll ins
    val _ = TextIO.closeIn ins
    val b = String.fields (fn c => c = #"\n") contents
    val d = map (String.fields (fn c => c = #" ") ) b
  in 
    d
  end


fun LastJuman str = 
  let
    val tmpName = OS.FileSys.tmpName () 
    val cmd = "echo " ^ str ^ "| jumanpp > " ^ tmpName 
    val _ = OS.Process.system cmd
    val ins = TextIO.openIn tmpName
    val contents = TextIO.inputAll ins
    val _ = TextIO.closeIn ins
    val b = String.fields (fn c => c = #"\n") contents
    val c = map (split) b
    val d = map (String.fields (fn c => c = #" ") ) c
  in 
    d
  end


fun JumanSemTy list = 
  let
    fun take lt = 
      case List.getItem lt of
        SOME t => t
      | NONE => ("NIL:", ["NIL:"]) 
    fun LeftCut str = 
      let
        val substr = Substring.full str
        val subresult = Substring.splitl (fn c => not (c = #":")) substr
        val result = Substring.string (#1 subresult)
      in
        result
      end
    val a = #1 (take list)
    val aa = #2 (take list)
    val b = #1 (take aa)
    val bb = #2 (take aa)
    val c = #1 (take bb)
    val cc = #2 (take bb)
    val d = #1 (take cc)
  in
    a, b, c, d)
  end


fun JumanSemTy list = 
  let
    fun LeftCut str = 
      let
        val substr = Substring.full str
        val subresult = Substring.splitl (fn c => not (c = #":")) substr
        val result = Substring.string (#1 subresult)
      in
        result
      end
    val sublist = map (LeftCut) list
    fun recordMake str ls = List.find (fn c => c = str) ls
    val subresult : jumanOutputSemTy = 
    {
      代表表記 = if recordMake "代表表記" sublist = SOME "代表表記" then else NONE
      カテゴリ = recordMake "カテゴリ" sublist = SOME "カテゴリ" then else NONE
      漢字読み = recordMake "漢字読み" sublist = SOME "漢字読み" then else NONE
      反義 = recordMake "反義" sublist = SOME "反義" then else NONE
    } 
    in
      subresult
    end

fun JumanSemTy list = 
  let
    fun hoge str = String.isPrefix str 

fun listTorecordSemTy list lastlist = 
  let 
    exception Empty
    fun listHdStOption lt = 
      case List.hd lt of
        "*" => NONE
      | t => SOME t
      (*fun listHdStOption nil = raise Empty
        | listlistHdStOption h :: t = 
          case h of
            "*" => NONE
          | i => SOME i*)
    fun listHdIntOption lt = 
      case Int.fromString (List.hd lt) of  
        SOME 0 => NONE
      | SOME t => SOME t
      | NONE => NONE
    fun listHdLastOption lt = 
      case List.hd lt of
        "NIL" => NONE
      | t => JumanSemTy lt(*要修正*)
    val no1 = List.hd list
    val list2 = List.tl list
    val no2 = List.hd list2
    val list3 = List.tl list2
    val no3 = List.hd list3
    val list4 = List.tl list3
    val no4 = listHdStOption list4
    val list5 = List.tl list4
    val no5 = listHdStOption list5
    val list6 = List.tl list5
    val no6 = listHdStOption list6
    val list7 = List.tl list6
    val no7 = listHdStOption list7
    val list8 = List.tl list7
    val no8 = listHdStOption list8
    val list9 = List.tl list8
    val no9 = listHdIntOption list9
    val list10 = List.tl list9
    val no10 = listHdStOption list10
    val list11 = List.tl list10
    val no11 = listHdIntOption list11
    val no12 : jumanOutputSemTy = listHdLastOption lastlist(*要修正*)
    val result : jumanOutputTy = 
      {
       1_表層形 = no1, 
       2_読み = no2,
       3_見出し語 = no3,
       4_品詞大分類 = no4,
       5_品詞大分類_ID = no5, 
       6_品詞細分類 = no6,
       7_品詞細分類_ID = no7,
       8_活用型 = no8,      
       9_活用型_ID = no9,
       10_活用形 = no10,
       11_活用形_ID = no11,
       12_意味情報 = no12
      }
  in
    result
  end
