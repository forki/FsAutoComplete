module Main

open System.Reflection

type FAW() =
  member x.newestFSharpCoreFullName() =
    let findFirst versions =
      let exists v =
        let fullName = sprintf "FSharp.Core, Version=%s, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a" v
        try
          let a = Assembly.ReflectionOnlyLoad fullName
          Some (a.Location, a.GlobalAssemblyCache)
        with _ -> None
      List.tryPick exists versions

    findFirst ["4.4.0.0"; "4.3.1.0"; "4.3.0.0"]

[<EntryPoint>]
let main _ =
  let faw = new FAW()
  printfn "%A" (faw.newestFSharpCoreFullName())
  0
