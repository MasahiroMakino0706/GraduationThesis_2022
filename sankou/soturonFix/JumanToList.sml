structure JumanToList = 
struct 
  fun Split moji = 
    let
      fun p s = print (Substring.string s ^ "\n")
      fun strTosubstr str = Substring.full str
      val substr = strTosubstr moji
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
    val subresult = List.tl (List.rev d)
    val result = List.rev (List.tl subresult)
  in 
    result
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
    val c = map (Split) b
    val d = map (String.fields (fn c => c = #" ") ) c
  in 
    d
  end


fun JumanSemTy list = 
  let
    fun makePair str = 
      let
        val substr = Substring.full str
        val (label, value) = Substring.splitl (fn c => not (c = #":")) substr
        val value = Substring.triml 1 value 
      in
        (Substring.string label, Substring.string value)
      end
    val sublist = map (makePair) list
    fun assoc key list = List.find (fn (a, b) => a = key) list 
    val result = 
      case List.hd list = "NIL" of 
        true => NONE
      | false => SOME
    {
      代表表記 = case assoc "代表表記" sublist of 
                  SOME (key, value) => SOME value
                | NONE => NONE,
      カテゴリ = case assoc "カテゴリ"  sublist of
                  SOME (key, value) => SOME value
                | NONE => NONE,
      漢字読み = case assoc "漢字読み" sublist of 
                  SOME (key, value) => SOME value
                | NONE => NONE,
      反義 = case assoc "反義" sublist of 
             SOME (key, value) => SOME value
           | NONE => NONE
    } 
    in
      result
    end
end