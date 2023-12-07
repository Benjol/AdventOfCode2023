module AdventCommon

open System

let Split (sep:Char) (str:string) = str.Split(sep, StringSplitOptions.RemoveEmptyEntries)
let Split2 (sep1:Char) (sep2:Char) (str:string) = str.Split([|sep1;sep2|], StringSplitOptions.RemoveEmptyEntries)