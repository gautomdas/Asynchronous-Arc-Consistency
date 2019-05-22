# AAC3

The name of this project is the asynchronous arc consistency [algorithm] #3. The generic version for ac3 algorithm can be found in *Artificial Intelligence: A Modern Approach*[^1]. 

An overview of the files found in this project:

/dependencies - contains all the additional packages need to run the program.

/dependencies/quicklisp - package manager for lisp.



General Project Plan:

```flow
st=>start: Input Logic Puzzle Strings
op=>operation: Tokenize Strings in Constraints
op2=>operation: Generate Constraint Search Space
op3=>end: Use AC3 Algorithm w/ Async
e=>end

st->op->op2->op3->op4
```



```

```





[^1]:[Russell, Stuart](https://en.wikipedia.org/wiki/Stuart_J._Russell); [Norvig, Peter](https://en.wikipedia.org/wiki/Peter_Norvig). *Artificial Intelligence: A Modern Approach*. Prentice Hall.

