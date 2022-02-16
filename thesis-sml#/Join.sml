structure Join = 
struct
    fun listjoin (lis:Dynamic.dynamic list list) = 
      case lis of
        nil => raise Fail "nil to listJoin"
      | h :: nil => h
      | x :: y :: t => listjoin (_join(x, y) :: t)
end