module IO.Printf

toOctal : Int -> String
toOctal n = let (q, r) = (div n 8, mod n 8) in
            case q == 0 of
              False => toOctal q ++ show r
              True => show r

toHex : Int -> String
toHex n = let (q, r) = (div n 16, mod n 16) in
          case q == 0 of
            False => toHex q ++ show' r
            True => show' r
          where
            show' : Int -> String
            show' r = case r < 10 of
                        True => show r
                        False => singleton $ chr (r + 55)

-- This is a recursive type for describing a format specifier
-- For example, "%s is %d%" would be expressed as
-- Str (Lit " is " (Number (Lit "%" End)))
data Format = Number Format
            | Oct Format
            | Hex Format
            | Str Format
            | Chr Format
            | Dbl Format
            | Lit String Format
            | End

-- This function takes a format specifier in the form of a string
-- and recursively builds and returns a Format type
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'o' :: chars) = Oct (toFormat chars)
toFormat ('%' :: 'x' :: chars) = Hex (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: chars)        = Lit "%" (toFormat chars)
toFormat (c :: chars)          = case toFormat chars of
                                   Lit lit chars' => Lit (strCons c lit) chars'
                                   fmt => Lit (strCons c "") fmt

-- This is also a recursive type that describes
-- the return type of the printf function
PrintfType : Format -> Type
PrintfType (Number fmt)  = (i : Int) -> PrintfType fmt
PrintfType (Oct fmt)     = (i : Int) -> PrintfType fmt
PrintfType (Hex fmt)     = (i : Int) -> PrintfType fmt
PrintfType (Str fmt)     = (str : String) -> PrintfType fmt
PrintfType (Chr fmt)     = (char : Char) -> PrintfType fmt
PrintfType (Dbl fmt)     = (dbl : Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End           = String

-- This function is a helper for the printf function
-- and is actually the one that does all the work by
-- accumulating the output string
printfHelper : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfHelper (Number fmt) acc  = \i => printfHelper fmt $ acc ++ show i
printfHelper (Oct fmt) acc     = \i => printfHelper fmt $ acc ++ toOctal i
printfHelper (Hex fmt) acc     = \i => printfHelper fmt $ acc ++ toHex i
printfHelper (Str fmt) acc     = \s => printfHelper fmt $ acc ++ s
printfHelper (Chr fmt) acc     = \c => printfHelper fmt $ pack $ unpack acc ++ [c]
printfHelper (Dbl fmt) acc     = \d => printfHelper fmt $ acc ++ show d
printfHelper (Lit str fmt) acc = printfHelper fmt $ acc ++ str
printfHelper End acc           = acc

-- The actual interface and point of entry, it passes an empty
-- accumulator to the helper above
printf : (fmt : String) -> PrintfType $ toFormat $ unpack fmt
printf fmt = printfHelper _ ""
