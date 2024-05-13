# A# Language Reference

This is a document describing the variant of the A# language used by the zishzingers compiler.

## Built-ins

- stackalloc(BYTE_AMOUNT)
  - This allocates a set amount of bytes in the function's "stack" and returns the pointer of the space

## Differences from Aidan's compiler

- Decimal integer literals greater than an `s32` (but which can fit into a `u32`) do not get automatically bitcasted from `u32` -> `s32`. You must use a hex literal.
- `if` statements can only accept `bool`s, not `s32`s. This means you should do `val != 0`.
- Integer/float literal types are compile time only, you cannot use an integer or float literal to define the type of a variable. eg `let var = 1;` will not compile, you must do `let var: s32 = 1;` or `let var = (s32)1;`. This allows more advanced compile time math semantics. This also forces you to be a bit more verbose in your types, which is a plus for readability.
- Properties have completely changed syntax, they use the C# style syntax now.
- Forward declarations have been added, this is how the standard library works in zishzingers.
- Enums are now C# style, not JavaScript style, eg `member = 0` instead of `member: 0`

## Expression grammar

```
expression          → assignment ;
assignment          → logical_or "=" assignment
                    | logical_or ;
logical_or          → logical_and ( "||" logical_and )* ;
logical_and         → equality ( "&&" equality ) ;
equality            → comparison ( ( "!=" | "==" ) comparison )* ;
comparison          → cast ( ( ">" | ">=" | "<" | "<=" ) cast )* ;
cast                → bitwise ( "as" TYPE_NAME )* ;
bitwise             → term ( ( "&" | "^" | "|" ) term )* ;
term                → factor ( ( "-" | "+" ) factor )* ;
factor              → unary ( ( "/" | "*" ) unary )* ;
unary               → ( "!" | "-" ) unary
                    | dot ;
dot                 → primary ( "." ( FIELD | function_call | "*" ) )* ;
primary             → "true" | "false" | "null"
                    | "(" expression ")"
                    | INTEGER_LITERAL
                    | FLOAT_LITERAL
                    | TYPE_NAME
                    | vec2_construction
                    | vec3_construction
                    | vec4_construction
                    | function_call 
                    | guid_literal
                    | hash_literal
                    | wide_string_literal
                    | string_literal
                    | VARIABLE_ACCESS ;
hash_literal        → hSHA1HASH ;
guid_literal        → gNUMBER ;
vec2_construction   → "float2(" expression "," expression ")"
vec3_construction   → "float3(" expression "," expression "," expression ")"
vec4_construction   → "float4(" 
                      expression "," 
                      expression "," 
                      expression "," 
                      expression ")"
function_call       → FUNCTION_NAME "(" 
                      expression
                      | ( expression "," )+ expression
                      ")" ;
wide_string_literal → "L" string_literal
string_literal      → "'" STRING "'"
```
