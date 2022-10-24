exception InvalidAction(string)
exception InvalidFormat(string)
exception InvalidInput(string)

type format = Class | Method | Variable

let testCases = [
  "S;M;plasticCup()",
  "C;V;mobile phone",
  "C;C;coffee machine",
  "S;C;LargeSoftwareBook",
  "C;M;white sheet of paper",
  "S;V;pictureFrame",
]

let isUpperCase = ch => Js.String2.toUpperCase(ch) == ch
let stripSpaceAndCapitalize = s => {
  let capitalizeNext = ref(false)
  s
  ->Js.String2.split("")
  ->Belt.Array.map(ch => {
    if capitalizeNext.contents {
      capitalizeNext := false
      Js.String2.toUpperCase(ch)
    } else if ch == " " {
      capitalizeNext := true
      ""
    } else {
      ch
    }
  })
  ->Js.Array2.joinWith("")
  ->Js.String2.trim
}

let handleSplit = rawString => {
  rawString
  ->Js.String2.split("")
  ->Js.Array2.filter(ch => ch != "(" && ch != ")")
  ->Belt.Array.map(ch =>
    isUpperCase(ch) ? ` ${Js.String2.toLowerCase(ch)}` : Js.String2.toLowerCase(ch)
  )
  ->Js.Array2.joinWith("")
  ->Js.String2.trim
}

let handleCombine = (formatOption, rawString: string) => {
  switch formatOption {
  | Class => {
      let modifiedString =
        rawString->Js.String2.charAt(0)->Js.String2.toUpperCase ++
          rawString->Js.String2.sliceToEnd(~from=1)
      stripSpaceAndCapitalize(modifiedString)
    }

  | Method => stripSpaceAndCapitalize(rawString) ++ "()"
  | Variable => stripSpaceAndCapitalize(rawString)
  }
}

let main = rawInput => {
  let (rawAction, rawFormat, rawString) = switch rawInput->Js.String2.split(";") {
  | [rawAction, rawFormat, rawString] => (rawAction, rawFormat, rawString)
  | _ => raise(InvalidInput(rawInput))
  }

  let f = switch rawFormat {
  | "C" => Class
  | "M" => Method
  | "V" => Variable
  | _ => raise(InvalidFormat(rawFormat))
  }

  switch rawAction {
  | "S" => handleSplit(rawString)
  | "C" => handleCombine(f, rawString)
  | _ => raise(InvalidAction(rawAction))
  }
}
for i in 0 to testCases->Belt.Array.length - 1 {
  Js.log(main(testCases[i]))
}
