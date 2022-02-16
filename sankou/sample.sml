datatype bun = 動詞文 | 名詞文 | 形容詞文
val zouHaHanaGaNagai =
{
  種類 = 形容詞文,
  述語 = {形容詞 = "長い", 主格 = {名詞 = "鼻",の格 = "象"}},
  堤題 = {名詞 = "象"}
}
val zouHaHayakuHashiru =
{
  種類 = 動詞文,
  述語 = {動詞 = "走る", 主格 = {名詞 = "象"}, 連用修飾 = "速く"},
  堤題 = {名詞 = "象"}
}
val zouHa1 =
{
  種類 = 形容詞文,
  述語 = {主格 = {の格 = "象"}},
  堤題 = {名詞 = "象"}
}  
val zouHa2 =
{
  種類 = 動詞文,
  述語 = {主格 = {名詞 = "象"}},
  堤題 = {名詞 = "象"}
}  
val hanaGa1 = 
{
  種類 = 形容詞文,
  述語 = {主格 = {名詞 = "鼻"}}
}  
val hanaGa2 = 
{
  種類 = 動詞文,
  述語 = {主格 = {名詞 = "鼻"}}
}  
val nagai1 =
{
  種類 = 形容詞文,
  述語 = {形容詞 = "長い"}
}  
val hayaku1 =
{
  種類 = 動詞文,
  述語 = {連用修飾 = "速く"}
}  
val hashiru1 =
{
  種類 = 動詞文,
  述語 = {動詞 = "走る"}
}
val zouHa = [Dynamic.dynamic zouHa1, Dynamic.dynamic zouHa2]
val hanaGa = [Dynamic.dynamic hanaGa1, Dynamic.dynamic hanaGa2]
val nagai = [Dynamic.dynamic nagai1]
val hayaku = [Dynamic.dynamic hayaku1]
val hashiru = [Dynamic.dynamic hashiru1]
fun pr l = map (fn x => print ((Dynamic.toJson x) ^ "\n")) l
val zou1 = _join(zouHa, hanaGa)
val zou2 = _join(zou1, nagai)
val zou3 = _join(zouHa, hayaku)
val zou4 = _join(zou3, hashiru)
val _ = pr zou1
val _ = pr zou2
val _ = pr zou3
val _ = pr zou4



