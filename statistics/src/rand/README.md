Probability distribution random number generation method  
Author    Yotsutsuji Tetsuaki
    
Sorry we have not implemented all methods the plan is to grow little by little when we find the time.

For each distribution , if there is a random number generation method more than one , on which I wrote each another function ,
    
* Allegro Common Lisp 8.2 (x86-64) *
You have defined ( and compiler macro function ) wrapper to use the technique and was faster on the most

What 's that technique , or the fastest , I will depend on the speed expensive math functions ( trigonometric · log · sqrt , etc.)
Result of eliminating the expensive function in a complex way , that that change in the degree of trade-off which one will be faster
There is a possibility that the change is the fastest processing system is different


Tips

*-cached system function
In distribution with parameters , there is a function that has a value to be calculated " in advance " from parameter
In practical use , I want to generate a random number from a lot of the same parameters in many cases
Therefore, to calculate inside the function every time a critical value at the time the parameter has been determined it is a loss
-cached function of the system , reduces the computation in that it receives as arguments the critical values ​​of these

These functions are defined in a macro serving defun-with-cached-values
-cached and is used in the interior of the abstraction interface using a distribution object system function , (planned)

In addition , the argument passed if the value was not a variable ,-cached compiler macro wrapper , is replaced if available
Since already use this mechanism , the function is called a distribution random other internally , so that other functions that depend only by replacing the wrapper is also changed