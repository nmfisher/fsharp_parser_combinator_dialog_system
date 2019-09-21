namespace Aio.Dialog

open Parser
open System
open Newtonsoft.Json

// A grammar-based Dialog manager
module Dialog = 
  
  // The global context for the user's dialog. 
  let mutable (context:Map<string,string>) = Map.empty

  // The dialog state representing the list of responses expected/allowed from the user
  let mutable expected = [] :> string list

  // On load, the app will need to invoke this method to populate the context from a persistent datastore
  let populate (contextString:string) =
    match String.IsNullOrWhiteSpace contextString with 
    | true -> 
      ()
    | false -> 
      context <- JsonConvert.DeserializeObject<Map<string,string>> contextString

  // The invoking app will need to choose when to retrieve the context to persist back to the datastore
  // This will probably be on successful completion of a Conversation
  let fetchContext () =
    context

  // merge the provided context (m2) into the mutable context (m1)
  // in case of conflict, choose keys from m2
  let merge ctx =
    context <- ctx |> Map.fold (fun accum k v -> ctx.Add(k,v)) context

  // The invoking app may elect to reset the state to dispose of any active callbacks (e.g. if a Conversation is exited before completion)
  let reset () =
    expected <- []

  // Segment a string according to TextExpression boundaries
  let segment (accum:string list, remaining:string) expr =
    match expr with 
    | Text t ->
      let split = remaining.Split([|t|], System.StringSplitOptions())
      let next = split |> Array.skip 1
      split.[0] :: accum, String.Join("", next)
    | _ -> 
      accum, remaining 

  let isSatisfied (expr) = 
    match expr with 
    | Boolean b -> b.IsMetBy(context)
    | _ -> true

  // let allSatisfied (exprs:ExpressionType list) =
  //   match exprs |> Seq.tryFind not isSatisfiedCondition with 
  //   | Some -> false
  //   | None -> true

  // Parse each expected response into an Expression, then compare against the actual input to see if it matches
  // For example, in pseudocode
  // let expected = ["Hi <String[Bob|Mary] yourName>"]
  // let actual = "Hi Jane"
  // parse expected -> [Text("Hi "), Option(["Bob";"Mary"])
  // |> segment actual -> ["Jane", Option(["Bob"; Mary])]
  // this will evaluate to false (since "Jane" is not in the Option)
  // If any expected response starts with a Boolean Expression, these are first evaluated against the Context
  // If these evaluate false, they are discarded
  let tryValidate actual expected =

    // An expected response consists of three (optional) parts 
    // A Boolean Expression indicating whether the response is valid in the given Context
    // Text Expression(s) indicating text that the user must respond with verbatim
    // VariableDeclaration expressions ("slot candidates") indicating text that the user can choose, and that will be used to populate the Context under the respective variable key 
    let expressions = 
      expected 
      |> parse
      |> Seq.where (fun expr -> expr |> isSatisfied)

    if expressions |> Seq.isEmpty then
      failwith "No valid expressions were found in the given Context. This suggests a dialog scripting error."
    
    let slotCandidates = 
      expressions
      |> Seq.where (fun x -> x.IsDeclarationType) 
      |> Seq.map(fun x -> x.DeclarationValue)
   
    // split the input string on boundaries corresponding to each Text Expression
    // this returns a seq<string> representings the "slot responses" to each "slot candidate"
    let slotResponses = 
      expressions 
      |> Seq.fold segment ([], actual)  
      |> (fun (accum, remaining) -> match remaining.Length with | 0 -> accum | _ -> remaining :: accum)
      |> Seq.where (fun x -> x.Length > 0)

    // if the number of candidates doesn't match the number of responses, we know the response is invalid
    match Seq.length slotCandidates = Seq.length slotResponses with 
    | false -> 
      None
    | _ ->      
      // otherwise, check if every slotCandidate is matched by its corresponding slotResponse 
      // first, convert the slotCandidate/slotResponse pairs into a dictionary keyed by the slotCandidate key
      let lookup = slotCandidates |> Seq.map (fun x -> x.Key) |> Seq.zip <| slotResponses |> Map.ofSeq 
      // then check every pair           
      match slotCandidates |> Seq.tryFind (fun expr -> not (expr.IsMetBy(lookup))) with
      | Some _ -> 
        None
      | None ->
        Some(lookup)

  // Validate a response against the expected state
  let validate (actual:string) =
    match expected |> Seq.map (tryValidate actual) |> Seq.tryPick id with
    | None -> 
      Map.empty.Add("result", "false") |> JsonConvert.SerializeObject
    | Some lookup -> 
      merge lookup 
      Map.empty.Add("result", "true" :> obj).Add("context", context) |> JsonConvert.SerializeObject

  // Choose the options to present to the user that the user can respond with
  // This parses each candidate into an Expression, and returns all Expressions satisfied by the given Context
  // Since this module tracks dialog state, we store these on each iteration so the user doesn't have to pass the "expected" back-and-forth
  // Throws an exception if no satisfied Expression is found
  let expect (candidates:string) =
    expected <- candidates 
                  |> JsonConvert.DeserializeObject<string[]> 
                  |> Seq.ofArray
                  |> Seq.where (parse >> (fun x -> x |> Seq.forall (fun y -> y.IsMetBy(context))))
                  |> Seq.toList
    expected            

  // Choose something to say to the user from a list of candidates
  // This parses each candidate into an Expression, then returns the Expression that is satisfied by the given Context
  // Plain text Expressions are automatically satisfied
  // Interpolation Expressions are satisfied if the lookup contains corresponding key/value pairs
  // Boolean Expressions (i.e Conditions) are satisfied if the Expression is met by the corresponding key/value pair
  let say (candidates:string) context =
    candidates 
    |> JsonConvert.DeserializeObject<string []> 
    |> Seq.where (parse >> (fun x -> x |> Seq.forall (fun y -> y.IsMetBy(context))))
    |> Seq.head