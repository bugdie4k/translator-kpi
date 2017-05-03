Depends on [small-tests](https://github.com/fedorough/small-tests)
and [my-cl-utils](https://github.com/bugdie4k/my-cl-utils)

The grammar is:

![Grammar](https://cloud.githubusercontent.com/assets/22116479/25653327/11c85d88-2ff5-11e7-9278-e17601f9d6cb.png "Grammar")
![Grammar](https://cloud.githubusercontent.com/assets/22116479/25653348/2429b81e-2ff5-11e7-8a84-63bc1bc76bb7.png "Grammar")

The actual grammar used for this translator is changed so that expressions can do more than add and subtract
(because i used [cmu-infix](https://github.com/rigetticomputing/cmu-infix) as expression-parser)
and also another statement was added by teachers request.

Added statement looks like this: 

```
<labels> --> LABELS: <labels-declaration> ;  
<labels-declaration> --> <digit> <digits>  
<digits> --> <digit> <digits> | <empty>  
```

