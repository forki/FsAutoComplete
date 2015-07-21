namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

module CompilerServiceInterface =
  type RequestOptions(opts, file, src) =
    member x.Options : FSharpProjectOptions = opts
    member x.FileName : string = file
    member x.Source : string = src
    member x.WithSource(source) =
      RequestOptions(opts, file, source)
  
  let withTimeout (timeOut: option<int>) (operation: Async<'x>) : Async<option<'x>> =
    match timeOut with
     | None -> async {
         let! result = operation
         return Some result
       }
     | Some timeOut -> async {
         let! child = Async.StartChild (operation, timeOut)
         try
           let! result = child
           return Some result
         with :? System.TimeoutException ->
           return None
       }

  /// Load times used to reset type checking properly on script/project load/unload. It just has to be unique for each project load/reload.
  /// Not yet sure if this works for scripts.
  //let fakeDateTimeRepresentingTimeLoaded proj = DateTime(abs (int64 (match proj with null -> 0 | _ -> proj.GetHashCode())) % 103231L)

  let isAScript fileName =
      let ext = Path.GetExtension fileName
      [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

  let ensureFSharpCore (options: string[]) =
    if options |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then options
    else
      let dirs = FSharpEnvironment.getDefaultDirectories (None, FSharpTargetFramework.NET_4_5 )
      [| yield! options
         match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
         | Some fn -> yield sprintf "-r:%s" fn
         | None ->
         match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
         | Some fn -> yield sprintf "-r:%s" fn
         | None -> () |]

  let getDeclarations (checker: FSharpChecker) (options: RequestOptions) =
    let parseResult =
      checker.ParseFileInProject(options.FileName, options.Source, options.Options)
      |> Async.RunSynchronously
    parseResult.GetNavigationItems().Declarations
