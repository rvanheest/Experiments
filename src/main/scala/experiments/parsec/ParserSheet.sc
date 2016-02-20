import experiments.parsec.StringParser._

val p1 = item replace 5
p1 parse "abc"

val p2 = item *> number
p2 parse "a5"

val p3 = item <* number
p3 parse "a5"

val p4 = item >> number
p4 parse "a5"

val p5 = item << number
p5 parse "a5"

val p6 = item.skipMany
p6 parse "aaabbbccc"
