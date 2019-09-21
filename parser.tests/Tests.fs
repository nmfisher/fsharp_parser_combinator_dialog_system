module Tests

open System
open Xunit
open Aio.Dialog.Parser

type Converter(output: Xunit.Abstractions.ITestOutputHelper) =
    inherit System.IO.TextWriter()
    override __.Encoding = stdout.Encoding
    override __.WriteLine message =
        output.WriteLine message
    override __.Write message =
        output.WriteLine message

type DialogTests(output : Xunit.Abstractions.ITestOutputHelper) =
  do new Converter(output) |> Console.SetOut

  [<Fact>]
  let ``Parse a string containing a Text Expression and a VariableDeclaration that takes any string`` () =
    let parsed = "Hello <String[] name>" |> parse 
    Assert.Equal(List.length parsed, 2)
    Assert.True(parsed |> List.head |> (fun x -> match x with | Text t -> true | _ -> false))
    Assert.True(parsed |> List.last |> (fun x -> match x with | VariableDeclaration v -> true | _ -> false))
    Assert.True(parsed |> List.last |> (fun x -> x.IsMetBy(Map.empty.Add("name","FOO"))))
    Assert.True(parsed |> List.last |> (fun x -> x.IsMetBy(Map.empty.Add("name","BAR"))))

  [<Fact>]
  let ``Parse a string containing a Text Expression and a VariableDeclaration that takes a constrained string`` () =
    let parsed = "<String[FOO|BAR] name>, Hello" |> parse 
    Assert.Equal(List.length parsed, 2)
    Assert.True(parsed |> List.head |> (fun x -> match x with | VariableDeclaration v -> true | _ -> false))
    Assert.True(parsed |> List.head |> (fun x -> x.IsMetBy(Map.empty.Add("name","FOO"))))
    Assert.True(parsed |> List.head |> (fun x -> x.IsMetBy(Map.empty.Add("name","BAR"))))
    Assert.False(parsed |> List.head |> (fun x -> x.IsMetBy(Map.empty.Add("name","BAZ"))))
    Assert.True(parsed |> List.last |> (fun x -> match x with | Text t -> true | _ -> false))

  [<Fact>]
  let ``Parse a string containing a Boolean and a Text Expression`` () =
    let parsed = "<?motherAge eq 40>Hello <!motherName>" |> parse 
    Assert.Equal(List.length parsed, 3)
    printfn "%A" parsed
    Assert.True(parsed |> List.head |> (fun x -> x.IsMetBy(Map.empty.Add("motherAge","40"))))
    Assert.True(parsed |> List.last |> (fun x -> x.IsMetBy(Map.empty.Add("motherName","Mary"))))
