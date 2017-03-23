module Applicative.Core

open Chessie.ErrorHandling
open System.Text.RegularExpressions

type Customer = {Id: int; Name: string; Email: string}

let validateId id = if id > 0 then ok id else fail "Invalid Id"

let validateName (name: string) = if name.Length > 2 then ok name else fail "name too short"

let validateEmail (email: string) = 
    let regex = Regex @"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$"
    match regex.IsMatch(email) with
    | true -> ok email
    | false -> fail "email not valid"
    

let createCustomer id name email =
    let create = fun i n e -> {Id = i; Name = n; Email = e}

    create
    <!> validateId id
    <*> validateName name
    <*> validateEmail email
