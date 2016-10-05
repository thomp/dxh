# dxh

*HTML generation.*

---

## Write to a stream with

```lisp
(name output-generator stream)
```

An example:

```lisp
(with-output-to-string (s)
  (dxh:pre #'(lambda (pre-stream)
	       (write-string "glurp" pre-stream))
	   s))
"<pre>glurp</pre>"
```


## Generate a string with

```lisp
(name-s inner-string)
```

An example:

```