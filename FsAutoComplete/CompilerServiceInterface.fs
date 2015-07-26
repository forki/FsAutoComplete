namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module CompilerServiceInterface =
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

  let getDeclarations (checker: FSharpChecker) fileName source options =
    let parseResult =
      checker.ParseFileInProject(fileName, source, options)
      |> Async.RunSynchronously
    parseResult.GetNavigationItems().Declarations

  let tryGetProjectOptions (checker: FSharpChecker) (file: string) : Result<_> =
    if not (File.Exists file) then
      Failure (sprintf "File '%s' does not exist" file)
    else
      try
        let p = FSharpProjectFileInfo.Parse(file)
        let args = p.Options |> Array.ofList

        let projectOptions = checker.GetProjectOptionsFromCommandLineArgs(file, args)
        let referencedProjectOptions =
          [| for file in p.ProjectReferences do
               yield file, checker.GetProjectOptionsFromProjectFile(file) |]

        let po =
          { projectOptions
            with ReferencedProjects = referencedProjectOptions }
        Success (po, p.CompileFiles, p.OutputFile, p.References, p.FrameworkVersion)
      with e ->
        Failure e.Message

  let tryGetMethodOverrides (tyRes: FSharpCheckFileResults) (lines: string[]) (line: int) (col: int) =
    // Find the starting point, ideally right after the first '('
    let lineCutoff = line - 3
    let commas, line, col =
      let rec prevPos (line,col) =
        match line, col with
        | 1, 1
        | _ when line < lineCutoff -> 1, 1
        | _, 1 ->
           let prevLine = lines.[line - 2]
           if prevLine.Length = 0 then prevPos(line-1, 1)
           else line - 1, prevLine.Length
        | _    -> line, col - 1

      let rec loop commas depth (line, col) =
        if (line,col) <= (1,1) then (0, line, col) else
        let ch = lines.[line - 1].[col - 1]
        let commas = if depth = 0 && ch = ',' then commas + 1 else commas
        if (ch = '(' || ch = '{' || ch = '[') && depth > 0 then loop commas (depth - 1) (prevPos (line,col))
        elif ch = ')' || ch = '}' || ch = ']' then loop commas (depth + 1) (prevPos (line,col))
        elif ch = '(' || ch = '<' then commas, line, col
        else loop commas depth (prevPos (line,col))
      match loop 0 0 (prevPos(line,col)) with
      | _, 1, 1 -> 0, line, col
      | newPos -> newPos

    let lineStr = lines.[line - 1]
    match Parsing.findLongIdentsAtGetMethodsTrigger(col - 1, lineStr) with 
    | None -> Failure "Could not find ident at this location"
    | Some identIsland ->

    let meth = tyRes.GetMethodsAlternate(line, col, lineStr, Some identIsland)
               |> Async.RunSynchronously

    Success(meth, commas)
  
  let tryFindDeclaration (tyRes: FSharpCheckFileResults) line col lineStr =
    match Parsing.findLongIdents(col - 1, lineStr) with
    | None -> Failure "Could not find ident at this location"
    | Some(col,identIsland) ->

      let declarations = tyRes.GetDeclarationLocationAlternate(line, col + 1, lineStr, identIsland, false)
                         |> Async.RunSynchronously
      match declarations with
      | FSharpFindDeclResult.DeclNotFound _ -> Failure "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range -> Success range

  let tryGetToolTip (tyRes: FSharpCheckFileResults) line col lineStr =
    match Parsing.findLongIdents(col - 1, lineStr) with
    | None -> Failure "Cannot find ident for tooltip"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let tip = tyRes.GetToolTipTextAlternate(line, col + 1, lineStr, identIsland, Parser.tagOfToken(Parser.token.IDENT("")))
                |> Async.RunSynchronously
      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall (function
        FSharpToolTipElement.None -> true | _ -> false) ->
          Failure "No tooltip information"
      | _ -> Success(tip)

  let tryGetSymbolUse (tyRes: FSharpCheckFileResults) line col lineStr =
    async {
        match Parsing.findLongIdents(col - 1, lineStr) with
        | None -> return (Failure "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = tyRes.GetSymbolUseAtLocation(line, colu + 1, lineStr, identIsland)
        match symboluse with
        | None -> return (Failure "No symbol information found")
        | Some symboluse ->

        let! symboluses = tyRes.GetUsesOfSymbolInFile symboluse.Symbol
        return Success symboluses }
    |> Async.RunSynchronously

  let tryGetCompletions parseResults (tyRes: FSharpCheckFileResults) line col lineStr timeout filter =
    let longName, residue = Parsing.findLongIdentsAndResidue(col - 1, lineStr)
    try
      let results =
        Async.RunSynchronously (tyRes.GetDeclarationListInfo(Some parseResults, line, col, lineStr, longName, residue, fun (_,_) -> false),
                                ?timeout = timeout)
      let decls =
        match filter with
        | Some "StartsWith" -> [| for d in results.Items do if d.Name.StartsWith residue then yield d |]
        | Some "Contains" -> [| for d in results.Items do if d.Name.Contains residue then yield d |]
        | _ -> results.Items
      Some (decls, residue)
    with :? TimeoutException -> None

