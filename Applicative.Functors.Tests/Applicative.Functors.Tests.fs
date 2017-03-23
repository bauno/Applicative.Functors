module Applicative.Functors.Tests

open Xunit 
open FsUnit.Xunit
open System.Linq
open Chessie.ErrorHandling

[<Fact>]
let ``lift should multiply inner value of success by 2``() =
    ok 16
    |> Trial.lift ((*) 2) 
    |> should equal (ok 32)

let extractError =
    function
    | Ok(n, _) -> failwith "Error: I was expecting an error :-)"
    | Bad s -> s.[0]

let doublex x =
     x * 2

[<Fact>]
let ``lift should multiply inner value of success by 2 using infix operator``() =

    // (<!>) ((*) 2) (ok 16)
    // ((*) 2) <!> (ok 16)
    doublex <!> ok 16
    |> should equal (ok 32)

[<Fact>]
let ``lift applied to a failure should fail``() =
    fail "error"
    |> Trial.lift doublex
    |> extractError
    |> should equal "error"

[<Fact>]
let ``lift applied to a failure using infix operator should fail``() =
    doublex <!> fail "Infix error"
    |> extractError
    |> should equal "Infix error"
   


