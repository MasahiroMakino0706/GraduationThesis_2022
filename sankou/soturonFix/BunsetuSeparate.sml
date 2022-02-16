structure BunsetuSeparate = 
struct 
fun separate record lis = 
  if lis = nil then
    case #4_品詞大分類 record of
      "形容詞" => [Type.形容詞 {形容詞 = #1_表層形 record, 形容詞情報 = record}]
    | "動詞" => [Type.動詞 {動詞 = #1_表層形 record, 動詞情報 = record}]
    | "名詞" => [Type.名詞 {名詞 = #1_表層形 record, 名詞情報 = record}]
    | _ => raise Fail "unknown hinshi"
  else
    if #4_品詞大分類 (List.hd lis) = "助詞" then
      case #1_表層形 (List.hd lis) of
        "は" => List.@ ([Type.名詞は {名詞 = #1_表層形 record, 名詞情報 = record, 助詞情報 = List.hd lis}], separate (List.hd (List.tl lis)) (List.tl (List.tl lis)))
      | "が" => List.@ ([Type.名詞が {名詞 = #1_表層形 record, 名詞情報 = record, 助詞情報 = List.hd lis}], separate (List.hd (List.tl lis)) (List.tl (List.tl lis)))
      | "の" => List.@ ([Type.名詞の {名詞 = #1_表層形 record, 名詞情報 = record, 助詞情報 = List.hd lis}], separate (List.hd (List.tl lis)) (List.tl (List.tl lis)))
      | "に" => List.@ ([Type.名詞に {名詞 = #1_表層形 record, 名詞情報 = record, 助詞情報 = List.hd lis}], separate (List.hd (List.tl lis)) (List.tl (List.tl lis)))
      | "を" => List.@ ([Type.名詞を {名詞 = #1_表層形 record, 名詞情報 = record, 助詞情報 = List.hd lis}], separate (List.hd (List.tl lis)) (List.tl (List.tl lis)))
      | "へ" => List.@ ([Type.名詞へ {名詞 = #1_表層形 record, 名詞情報 = record, 助詞情報 = List.hd lis}], separate (List.hd (List.tl lis)) (List.tl (List.tl lis)))
      | _ => raise Fail "unknown hinshi"
    else
      case #4_品詞大分類 record of
        "形容詞" => List.@ ([Type.形容詞 {形容詞 = #1_表層形 record, 形容詞情報 = record}], separate (List.hd lis) (List.tl lis))
      | "動詞" => List.@ ([Type.動詞 {動詞 = #1_表層形 record, 動詞情報 = record}], separate (List.hd lis) (List.tl lis))
      | "名詞" => List.@ ([Type.名詞 {名詞 = #1_表層形 record, 名詞情報 = record}], separate (List.hd lis) (List.tl lis))
      | _ => raise Fail "unknown hinshi"


fun undone t =
  case t of
    Type.名詞は s => 
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {名詞 = #名詞 s}},
            提題 = {名詞 = #名詞 s}
          },
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {の格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {に格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {を格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {名詞 = #名詞 s}},
            提題 = {名詞 = #名詞 s}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {の格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {に格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {を格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {名詞 = #名詞 s}},
            提題 = {名詞 = #名詞 s}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {の格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {に格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {を格 = #名詞 s}},
            提題 = {名詞 = #名詞 s}             
          }                              
      ]
  | Type.名詞が s => 
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {名詞 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {名詞 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {名詞 = #名詞 s}}
          }
      ]
  | Type.名詞の s =>
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {の格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {の格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {の格 = #名詞 s}}
          }
      ]
  | Type.名詞に s =>
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {に格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {に格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {に格 = #名詞 s}}
          }
      ]
  | Type.名詞を s =>
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {を格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {を格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {を格 = #名詞 s}}
          }
      ]
  | Type.名詞へ s =>
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {主格 = {へ格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {へ格 = #名詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {へ格 = #名詞 s}}
          }
      ]
  | Type.形容詞 s =>
      [
        Dynamic.dynamic
          {
            種類 = "形容詞文",
            述語 = {形容詞 = #形容詞 s}
          },
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {主格 = {連用修飾 = #形容詞 s}}
          },
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {主格 = {連用修飾 = #形容詞 s}}
          }
      ]
  | Type.動詞 s =>
      [
        Dynamic.dynamic
          {
            種類 = "動詞文",
            述語 = {動詞 = #動詞 s}
          }
      ]
  | Type.名詞 s =>
      [
        Dynamic.dynamic
          {
            種類 = "準詞文",
            述語 = {名詞 = #名詞 s}
          }
      ]
end

