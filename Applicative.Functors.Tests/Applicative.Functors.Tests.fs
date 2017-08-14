 module Applicative.Functors.Tests

open Xunit
open FsUnit.Xunit
open System.Linq
open Chessie.ErrorHandling
open Applicative.Core
open Swensen.Unquote

[<Fact>]
let ``lift should multiply inner value of success by 2``() =
    ok 16
    |> Trial.lift ((*) 2)
    |> should equal (ok 32)

let extractError =
    function
    | Ok(n, _) -> failwith "Error: I was expecting an error :-)"
    | Bad s -> s

let extractOk =
    function
    | Ok(n,_) -> n
    | Bad _ -> failwith "Error: I was expecting ok!"

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
    |> should equal ["error"]

[<Fact>]
let ``lift applied to a failure using infix operator should fail``() =
    doublex <!> fail "Infix error"
    |> extractError
    |> should equal ["Infix error"]


[<Fact>]
let ``bind should bypass after first error``() =
    let validate c = ok c
    let update c = fail "update error"
    let send c = fail "send error"

    let customer = { Id = 42; Name = "John"; Email = "john@example.com" }

    validate customer
    >>= update
    >>= send
    |> extractError
    |> should equal ["update error"]

[<Fact>]
let ``bind should create valid customer if no failures``() =
    let validate c = ok c
    let update c = ok c
    let send c = ok c

    let customer = { Id = 42; Name = "John"; Email = "john@example.com" }

    validate customer
    >>= update
    >>= send
    |> should equal (ok customer)

[<Fact>]
let ``apply valid inputs should create valid customer``() =
    createCustomer 42 "John" "john@example.com"
    |> extractOk
    |> should equal { Id = 42; Name = "John"; Email = "john@example.com" }

[<Fact>]
let ``apply invalid inputs should fail with accumulated messages``() =    

    let expected = Bad ([ "Invalid Id"; "email not valid" ]) : Result<Customer, string>
    let res = createCustomer -1 "John" "foo" 
    
    res |> should equal expected
    test <@ expected  = res@>    

    
    
