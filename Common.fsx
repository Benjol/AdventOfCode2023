module AdventCommon

open System

let getlines (input:string) = input.Split([|'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries)