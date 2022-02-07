object hw1 extends eecs.cs385:
  def userName = "Christopher Eno"

  // MY DOCUMENTATION IDENTIFIES ALL SOURCES USED AND ASSISTANCE RECEIVED
  // IN COMPLETING THIS ASSIGNMENT.
  // Christopher Eno

  def cubeRoot(n: Long): Long =

    // Runtime: Olog(n)
  
    var lo : Long = 0
    var hi = n

    while lo < hi do
        val mid = (lo+hi)/2
        if mid*mid*mid < n then lo = mid + 1
        else hi = mid
    lo

    // Recursive
    def root3(n: Int, lo: Int, hi: Int): Long = 
        if lo >= hi then hi
        else
            mid = (lo+hi)/2
            if mid^3 < n then root3(n, mid+1, hi)
            else root3(n, lo, mid)
    

  test("cubeRoot", cubeRoot, "n")

  //////////////////////////////////////////////////////////////////////

  def lastIndexOf(x: Int, arr: Array[Int]): Option[Int] =
    
    var lo = 0
    var hi = arr.length - 1
    while lo < hi do
        mid = (lo+hi)/2
        if arr(mid) > x then hi = mid - 1
        else lo = mid
    if array(lo) == x then Some(lo)
    else None
    
    
    // Runtime: I'm not sure if this is Olog(n) because the method I used to deal with
    // duplicates is linear, which could take a long time in the worst case. However,
    // the majority of the searching is done in log time.

    val length = arr.length - 1
    var mid = length / 2
    var lo = 0
    var hi = length

    while lo <= hi do
      if arr(mid) < x then
        lo = mid + 1
        mid = (lo+hi)/2
      else if arr(mid) > x then
        hi = mid - 1
        mid = (lo+hi)/2
      else
        while arr.length > mid + 1 && arr(mid + 1) == x do
          mid += 1
        return Some(mid)
    None

  test("lastIndexOf", lastIndexOf, "x", "arr")

  //////////////////////////////////////////////////////////////////////

  def rotatedBy(arr: Array[Int]): Int =
    
    // Runtime: Olog(n)
    
    val length = arr.length - 1
    var mid = length / 2
    var lo = 0
    var hi = length
    if arr.length == 1 then 0
    else
      while mid > 0 && mid < length && lo <= hi do
        if arr(mid) < arr(mid-1) && arr(mid) < arr(mid+1) then return mid
        else if arr(mid) < arr(hi) then
          hi = mid - 1
          mid = (hi+lo)/2
        else
          lo = mid + 1
          mid = (hi+lo)/2
      if mid == 0 && arr(mid) > arr(mid+1) then return mid + 1
      else mid
    /*
    https://www.geeksforgeeks.org/find-rotation-count-rotated-sorted-array/
    I developed the partial solution below, but was having trouble with about 30 of the test cases.
    Reading the website, I realized that the conditions I was using were wrong. I was using
    conditions to check if the current element was between the elements to its left and right,
    which stopped me from going back left after I'd started going right, and vice versa. From this
    source, I learned that I should check whether the current element was the minimum, at which point
    I could stop looking and return it.
    */

    // var check = 1
    // while check == 1 && lo <= hi do
    //   if mid < arr.length - 1 && arr(mid) < arr(mid+1) && arr(mid) > arr(mid-1) then
    //     lo = mid + 1
    //     mid = (lo+hi)/2
    //   else if arr(mid) == arr.min then return mid
    //   else if mid == arr.length-1 then check = 0
    //   else if arr(mid) > arr(mid+1) then return mid + 1
    // mid = length/2
    // lo = 0
    // hi = length

    // while lo <= hi do
    //   if mid > 0 && arr(mid) < arr(mid+1) && arr(mid) > arr(mid-1) then
    //     hi = mid - 1
    //     mid = (lo+hi)/2
    //   else if arr(mid) > arr(mid+1) then return mid + 1
    //   else return mid
    // println("here")
    // mid

    

    



  test("rotatedBy", rotatedBy, "arr")

  //////////////////////////////////////////////////////////////////////
  // don't change this line
  @main def run = runActions
