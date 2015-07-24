def merge(comp, list1, list2):
	if list1 is empty:
		return list2
	if list2 is empty:
		return list1

	v1 = list1.first
	v2 = list2.first

	if v1 > v2:
		list1 = list1.second
		return Pair(v1, merge(comp, list1, list2))
	else:
		list2 = list2.second
		return Pair(v2, merge(comp, list1, list2))