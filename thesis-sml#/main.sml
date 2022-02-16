local
  open Dynamic
  fun mes s =
      print ( "*****************************"
            ^ s
            ^ "*****************************\n" )
  fun ppDynList l = app (fn x => print (toJson x ^ "\n")) l
in
  val _ = print("解析したい文を入力してください。\n")
  val 構文解析対象文 = case TextIO.inputLine TextIO.stdIn of 
                        NONE => "エラー"
                      | SOME t => String.extract (t, 0, SOME (String.size t - 1))
  val _ = mes "構文解析対象文"
  val _ = print (構文解析対象文 ^ "\n")
  val 形態素 = map (ListToRecord.listTorecordSemTy) (JumanToList.Juman 構文解析対象文)
  val _ = mes "形態素"
  val _ = pp 形態素
  val 文節 = BunsetuSeparate.separate (List.hd 形態素) (List.tl 形態素)
  val _ = mes "文節"
  val _ = pp 文節  
  val 不完全文構造 = map (BunsetuSeparate.undone) 文節
  val _ = mes "不完全文構造"
  val _ = pp 不完全文構造
  val 構文解析結果 = Join.listjoin 不完全文構造
  val _ = mes "構文解析結果"
  val _ = pp 構文解析結果
  val _ = mes "構文解析結果 in json form"
  val _ = ppDynList 構文解析結果
end