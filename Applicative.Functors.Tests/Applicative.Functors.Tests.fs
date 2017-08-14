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
let doublex x =
     x * 2

[<Fact>]
let ``lift should multiply inner value of success by 2 using infix operator``() =
    doublex <!> ok 16
    |> should equal (ok 32)

[<Fact>]
let ``lift applied to a failure should fail``() =
    fail<int,string> "error"
    |> Trial.lift doublex    
    |> should equal (Bad ["error"] : Result<int, string>)

[<Fact>]
let ``lift applied to a failure using infix operator should fail``() =
    doublex <!> fail<int,string> "Infix error"
    |> should equal (Bad ["Infix error"]: Result<int, string>)


[<Fact>]
let ``bind should bypass after first error``() =
    let validate c = ok c
    let update c = fail<Customer, string> "update error"
    let send c = fail<Customer, string> "send error"

    let customer = { Id = 42; Name = "John"; Email = "john@example.com" }

    validate customer
    >>= update
    >>= send
    |> should equal (Bad ["update error"] : Result<Customer, string>)

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
    |> should equal (ok { Id = 42; Name = "John"; Email = "john@example.com" } : Result<Customer, string>)

[<Fact>]
let ``apply invalid inputs should fail with accumulated messages``() =    

    let expected = Bad ([ "Invalid Id"; "email not valid" ]) : Result<Customer, string>
    let res = createCustomer -1 "John" "foo" 
    
    res |> should equal expected
    test <@ expected  = res@>    

    
    
