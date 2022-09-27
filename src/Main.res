@send external flatMap: (array<'a>, 'a => array<'b>) => array<'b> = "flatMap"

type parser<'a> = Parser(string => array<('a, string)>)

let parse = (Parser(p)) => p

let return = a => Parser(cs => [(a, cs)])

let then = (p, f) => Parser(cs => parse(p, cs)->flatMap(((a, cs')) => parse(f(a), cs')))

let token = Parser(
  cs =>
    switch cs {
    | "" => []
    | cs' => [(cs'->Js.String2.get(0), cs'->Js.String2.sliceToEnd(~from=1))]
    },
)
