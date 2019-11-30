This is a vscode extension for formatting `Racket` src files.

## Features

Use command `racket-pretty` or turn on "format on save" to use this ext.

before:
```
(
    define a            
 2           )

 (multiple-value-bind (a b)

    
    (gethash 
    'a *m*)
  

                (list        
           a        
          b
              ))
```

after:
```
(define a 2)

(multiple-value-bind (a b)
                     (gethash 'a *m*)
                     (list a b))
```

## Requirements

Make sure you have `racket-pretty-printer` in path.

> https://github.com/Shuumatsu/racket-pretty-printer


## Extension Settings

```
{
  "racket-pretty-printer.width": 160,
  "racket-pretty-printer.ribbon": 0.5,
}
```

## Known Issues

In very early stage, not fully compatible with `Racket` syntax yet. 

### For more information

See repo https://github.com/Shuumatsu/racket-pretty-printer

**Enjoy!**
