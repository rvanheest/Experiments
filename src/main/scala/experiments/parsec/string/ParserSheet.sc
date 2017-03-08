import experiments.parsec.string.StringParser._

val p1 = item as 5
p1 run "abc"

val p2 = item >> number
p2 run "a5"

val p3 = item << number
p3 run "a5"

val p4 = item.skipMany
p4 run "aaabbbccc"
