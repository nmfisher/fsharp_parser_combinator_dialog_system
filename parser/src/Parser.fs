namespace Aio.Dialog

// #r @"C:\Users\nickh\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsecCS.dll"
// #r @"C:\Users\nickh\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsec.dll"

open FParsec
open System

// Parse a string into an Expression
// An Expression is a representation of the textual content of the string, and any of the following:
// - plain old text (e.g. "Hello!")
// - a condition relating to the value of a Variable (e.g. "<?age eq 40>You're so young!")
// - the assignment of a value to a Variable (e.g. "My mother is <Int[1,100] age> years old")

module Parser =
  type Variable = 
  | String of string
  | Int of int
  
  type IExpression (key:string) = 
    abstract member IsMetBy : Map<string,string> -> bool
    default __.IsMetBy (context:Map<string,string>) = false
    member __.GetValue (context:Map<string,string>) = match context.ContainsKey(key) with | true -> Some(context.[key]) | _ -> None
    member __.Key = key

  type Interpolation (key:string) = 
    inherit IExpression(key) with
      override __.IsMetBy (lookup:Map<string,string>) = 
        lookup.ContainsKey(key)

  type Option(key:string, options:string) =
    inherit IExpression(key) with 
      let split = options.Split([|"|"|], StringSplitOptions())
      override __.IsMetBy (lookup:Map<string,string>) = 
        match split.Length = 1 && String.IsNullOrWhiteSpace(split.[0]) with 
        | true -> 
            true 
        | false -> 
            match __.GetValue lookup with 
            | None -> false
            | Some v -> split |> Seq.contains (v.ToString())

  type Range(key:string, rangeFrom:int, rangeTo:int) = 
    inherit IExpression(key) with 
      override __.IsMetBy (lookup:Map<string,string>) = 
        match __.GetValue lookup with 
        | None -> false
        | Some v -> 
          try 
            let intVal = Convert.ToInt32(v)
            intVal >= rangeFrom && intVal <= rangeTo
          with _ -> false        
      
  type Equality (key:string, object:Variable) =
    inherit IExpression(key) with 
      override __.IsMetBy (lookup:Map<string,string>) = 
        match __.GetValue lookup with 
        | None -> false
        | Some v -> 
            match object with 
            | String s -> object.ToString() = s
            | Int i -> try Convert.ToInt32(v) = i with _ -> false        
      member __.Value = object      
  
  type Inequality (key:string, object:'T) =
    inherit IExpression(key) with
      override __.IsMetBy (lookup:Map<string,string>) = 
        match __.GetValue lookup with 
        | None -> false
        | Some v -> 
            match object with 
            | String s -> object.ToString() <> s 
            | Int i ->  try  Convert.ToInt32(v) <> i with _ -> false          

  type GreaterThan (key:string, object:Variable) =
    inherit IExpression(key) with
      override __.IsMetBy (lookup:Map<string,string>) = 
        match __.GetValue lookup with 
        | None -> false
        | Some v -> 
          match object with 
          | String s -> failwith "Type error"
          | Int i -> try Convert.ToInt32(v) > i with _ -> false

  type LessThan (key:string, object:Variable) =
    inherit IExpression(key)
      override __.IsMetBy (lookup:Map<string,string>) = 
        match __.GetValue lookup with 
        | None -> false
        | Some v -> 
          match object with 
          | String s -> failwith "Type error"
          | Int i -> try Convert.ToInt32(v) < i with _ -> false
 
  type ExpressionType = 
  | Text of string
  | Boolean of IExpression
  | InterpolationStatement of Interpolation
  | VariableDeclaration of IExpression
    member __.IsMetBy lookup = match __ with | Text t -> true | Boolean b -> b.IsMetBy(lookup) | InterpolationStatement i -> i.IsMetBy(lookup) | VariableDeclaration v -> v.IsMetBy(lookup)
    member __.IsDeclarationType = match __ with | VariableDeclaration v -> true | _ -> false
    member __.DeclarationValue = match __ with | VariableDeclaration v -> v | _ -> failwith "Not declaration type"
    
  type Parser<'T> = Parser<'T, unit>
  let str s : Parser<_> = pstring s
  let openAngle = str "<"
  let closeAngle = str ">"
  let openBracket = str "["
  let closeBracket = str "]"
  let questionMark = str "?"
  let comma = str ","
  let space = str " "  

  let varName = str " " >>. charsTillString ">" true Int32.MaxValue
  let rangeStart = openBracket >>. pint32 .>> comma
  let rangeEnd = pint32 .>> closeBracket
  let range = rangeStart .>>. rangeEnd
  let rangeDef : Parser<ExpressionType> = skipString "Int" >>. pipe2 range varName (fun (valFrom,valTo) v -> Range(v, valFrom, valTo) :> IExpression) |>> VariableDeclaration
  let poption = openBracket >>. charsTillString "]" true Int32.MaxValue
  // let stringDef : Parser<ExpressionType> = skipString "String" >>. varName |>> (fun x -> FreeText(x) :> IExpression)  |>> VariableDeclaration
  let optionDef : Parser<ExpressionType> = skipString "String" >>. pipe2 poption varName (fun values key -> Option(key, values) :> IExpression) |>> VariableDeclaration
  let typedef = rangeDef <|> optionDef

  let condVarName : Parser<_> = charsTillString " " true Int32.MaxValue 
  
  let condValue : Parser<_> = (attempt pint32 .>> str ">" |>> Int) <|> (charsTillString ">" true Int32.MaxValue |>> String)
  
  let eq : Parser<_> = condVarName .>> str "eq" .>> str " " .>>. condValue |>> (fun x -> Equality(x) :> IExpression) |>> Boolean
  let ne : Parser<_> = condVarName .>> str "ne" .>> str " " .>>. condValue |>> (fun x -> Inequality(x) :> IExpression)  |>> Boolean
  let gt : Parser<_> = condVarName .>> str "gt" .>> str " " .>>. condValue |>> (fun x -> GreaterThan(x) :> IExpression)  |>> Boolean
  let lt : Parser<_> = condVarName .>> str "lt" .>> str " " .>>. condValue |>> (fun x -> LessThan(x) :> IExpression) |>>  Boolean
  let conditions = (attempt eq <|> attempt gt <|> attempt lt <|> attempt ne)
      
  let condition = openAngle >>. questionMark >>. conditions 
  let interpolation : Parser<_> = openAngle >>. str "!" >>. charsTillString ">" true Int32.MaxValue |>> Interpolation |>> InterpolationStatement
  let declaration = openAngle >>. typedef 

  let text : Parser<_> = many1Satisfy (fun c -> c <> '<') |>> Text
  let evaluate = (text <|> attempt condition <|> attempt declaration <|> attempt interpolation)

  let parse target = 
    let result = run (many evaluate) target 
    match result with
    | Success(parsed, _, _) ->
      parsed 
    | Failure(err, _, _) ->
      raise (Exception("Could not parse"))

 