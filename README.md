# INI file parser

Although there are many other .ini file parsers for Haskell (three of which are on Hackage), I decided
to wrote my own simplistic one.

It should parse most .ini files; the parser output is structured like this:

    [((Section name :: String,
       Key name :: String),
     Value :: Value)] :: [((String,String),Value)]

in which

    data Value = INIBool Bool | INIString String | ININumber Double

The parsed test file supplied in the project tree looks like this:

    [(("abc","def"),ghi),(("abc","jkl"),xyz 
            d),(("12x","foo"),bar),(("12x","answer"),42.0)]

The values lack quotation marks because of a custom `Show` instance.

## to Map

It is possible to convert the raw parser output to a `Data.Map` map using the `iniStructToMap` function:

```haskell
    ghci> (Right i) <- (readFile "test.ini" >>= return . parse (iniP) "test.ini")
    ghci> let m = iniStructToMap i
    ghci> Map.lookup "12x.foo" m
    Just bar
    ghci> Map.lookup "12x.answer" m
    Just 42.0
    ghci> Map.lookup "13x.answer" m
    Nothing
```

As you can see, the settings' names are each converted to `section.name`.
