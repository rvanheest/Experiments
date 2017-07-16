import experiments.dlist.DList

val list = DList.fromList(List(1, 2, 3))
val list2 = 0 :: list
val list3 = list2 :+ 4
