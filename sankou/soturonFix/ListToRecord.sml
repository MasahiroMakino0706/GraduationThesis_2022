structure ListToRecord = 
struct 
  fun listTorecordSemTy list = 
    let 
      exception Empty
      fun listHdStOption lt = 
        case List.hd lt of
          "*" => NONE
        | t => SOME t
      fun listHdIntOption lt = 
        case Int.fromString (List.hd lt) of  
          SOME 0 => NONE
        | SOME t => SOME t
        | NONE => NONE
      val no1 = List.hd list
      val list2 = List.tl list 
      val no2 = List.hd list2
      val list3 = List.tl list2
      val no3 = List.hd list3
      val list4 = List.tl list3
      val no4 = List.hd list4
      val list5 = List.tl list4
      val no5 = listHdIntOption list5
      val list6 = List.tl list5
      val no6 = List.hd list6
      val list7 = List.tl list6
      val no7 = listHdIntOption list7
      val list8 = List.tl list7
      val no8 = listHdStOption list8
      val list9 = List.tl list8
      val no9 = listHdIntOption list9
      val list10 = List.tl list9
      val no10 = listHdStOption list10
      val list11 = List.tl list10
      val no11 = listHdIntOption list11
      val sublist = List.tl list11
      fun ListSum ls = 
        case ls of
        h :: t => h ^ ListSum t
      | nil => ""
      val contents = ListSum sublist
      val a = JumanToList.Split contents
      val d = String.fields (fn c => c = #" ") a
      val no12 : Type.jumanOutputSemTy option  = JumanToList.JumanSemTy d
      val result : Type.jumanOutputTy = 
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
end