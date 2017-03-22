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

    let pippo =
        Trial.lift doublex
    
    let arg = fail "Lo zio!!"

    let pluto = pippo arg

    match pluto with
    | Ok(n,_) -> printfn "Pluto is %i" n
    | Bad s -> s.[0] |> should equal "Lo zio!!"

    printfn "Lo zio: %A" pluto
    pluto
    |> should equal (Bad["Lo zio"])

    let qui = Error "Lo zio"
        
    // fail "error"
    // |> Trial.lift ((*) 2) 
    // |> should equal (Error "error")


