//problem 2

def numDays(reservations: Array[(Int,Int)]): Int =
    type LoR = List[(Int, Int)]
    def mergesort(lo: Int, hi: Int): LoR =
        if lo == hi then List(reservations(lo))
        else
            val mid = (lo+hi)/2
            merge(mergesort(lo, mid), mergesort(mid+1, hi))
        
    def merge(lor1: LoR, lor2: LoR): LoR =
        if lor1.isEmpty then lor2
        else if lor2.isEmpty then lor1
        else if lor1.head._1 < lor2.head._1 then
            combine(lor1.head, merge(lor1.tail, lor2))
        else combine(lor2.head, merge(lor1, lor2.tail))

    def combine(res: (Int, Int), lor: LoR): LoR =
        if lor.isEmpty then List(res)
        else if res._2 < lor.head._1 then res :: lor
        else combine((res._1, res._2 max lor.head._2), lor.tail)

    if reservations.isEmpty then 0
    else mergesort(0, reservations.length-1).map(res => res._1)