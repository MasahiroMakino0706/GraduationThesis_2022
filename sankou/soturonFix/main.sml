local
  open Dynamic
  fun mes s = 
      print ( "*****************************" 
            ^ s 
            ^ "*****************************\n" )
  fun ppDynList l = app (fn x => print (toJson x ^ "\n")) l
in
  val moji = "象は速く走る"
  val result = map (ListToRecord.listTorecordSemTy) (JumanToList.Juman moji)
  val _ = mes "result"
  val _ = pp result
  val comfirm = BunsetuSeparate.separate (List.hd result) (List.tl result)
  val _ = mes "confirm"
  val _ = pp comfirm
  val comfirm2 = map (BunsetuSeparate.undone) comfirm
  val _ = mes "confirm2"
  val _ = pp comfirm2
(*
  val subjoin = Join.listjoin (List.hd comfirm2)
*)
  val subjoin = Join.listjoin comfirm2
  val _ = mes "subjoin"
  val _ = pp subjoin
  val _ = mes "subjoin in json form"
  val _ = ppDynList subjoin
end
