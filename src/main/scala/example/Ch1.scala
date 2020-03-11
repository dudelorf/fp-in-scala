package example

object Ch1 {
    
    def fib(n: Int): Int = {
    
        @annotation.tailrec
        def loop(num: Int, prevA: Int, prevB: Int): Int =
            if(num == n)
                prevA
            else
                loop(num + 1, prevB, prevA + prevB)
            
        
        loop(1, 0, 1)
    }
    
    def isSorted[A](arr: Array[A], p: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(arr: Array[A]): Boolean = arr match {
            case Array() | Array(_) =>
                true
            case Array(a, b) =>
                p(a, b)
            case _ if p(arr.head, arr.tail.head) =>
                loop(arr.tail)
            case _ =>
                false
        }
        loop(arr)
    }
    
    def stringTest = {
        val a1 = Array("aa", "bb", "cc")
        val a2 = Array("bb", "aa", "cc")
        
        def strSort(a: String, b: String) = a < b
        
        if(!isSorted(a1, strSort)) println("should return true for sorted")
        if(isSorted(a2, strSort)) println("should return false for not sorted")
    }
    
    def curry[A,B,C](f: (A, B) => C): A => (B => C) =
        (a: A) => (b: B) => f(a, b)
        
    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
        (a: A, b: B) => f(a)(b)
        
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        (a: A) => f(g(a))
}
