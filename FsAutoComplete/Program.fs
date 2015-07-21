// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

open Newtonsoft.Json
open Newtonsoft.Json.Converters

module internal CompletionUtils =
  let map =
    [ 0x0000,  ("Class", "C")
      0x0003,  ("Enum", "E")
      0x00012, ("Struct", "S")
      0x00018, ("Struct", "S") (* value type *)
      0x0002,  ("Delegate", "D")
      0x0008,  ("Interface", "I")
      0x000e,  ("Module", "N") (* module *)
      0x000f,  ("Namespace", "N")
      0x000c,  ("Method", "M")
      0x000d,  ("Extension Method", "M") (* method2 ? *)
      0x00011, ("Property", "P")
      0x0005,  ("Event", "e")
      0x0007,  ("Field", "F") (* fieldblue ? *)
      0x0020,  ("Field", "F") (* fieldyellow ? *)
      0x0001,  ("Field", "F") (* const *)
      0x0004,  ("Property", "P") (* enummember *)
      0x0006,  ("Exception", "X") (* exception *)
      0x0009,  ("Text File Icon", "t") (* TextLine *)
      0x000a,  ("Regular File", "R") (* Script *)
      0x000b,  ("Script", "s") (* Script2 *)
      0x0010,  ("Tip of the day", "t") (* Formula *);
      0x00013, ("Class", "C") (* Template *)
      0x00014, ("Class", "C") (* Typedef *)
      0x00015, ("Type", "T") (* Type *)
      0x00016, ("Type", "T") (* Union *)
      0x00017, ("Field", "F") (* Variable *)
      0x00019, ("Class", "C") (* Intrinsic *)
      0x0001f, ("Other", "o") (* error *)
      0x00021, ("Other", "o") (* Misc1 *)
      0x0022,  ("Other", "o") (* Misc2 *)
      0x00023, ("Other", "o") (* Misc3 *) ] |> Map.ofSeq

  /// Translates icon code that we get from F# language service into a MonoDevelop icon
  let getIcon glyph =
    match map.TryFind (glyph / 6), map.TryFind (glyph % 6) with
    | Some(s), _ -> s // Is the second number good for anything?
    | _, _ -> ("", "")



// --------------------------------------------------------------------------------------
// Main application command-line loop
// --------------------------------------------------------------------------------------

/// Represents current state
type internal State =
  {
    Files : Map<string,VolatileFile> //filename -> lines * touch date
    FileCheckOptions : Map<string, FSharpProjectOptions>
    HelpText : Map<String, FSharpToolTipText>
  }

  static member Initial =
    { Files = Map.empty
      FileCheckOptions = Map.empty
      HelpText = Map.empty }

/// Contains main loop of the application
module internal Main =
  open FsAutoComplete.CommandInput
  open FsAutoComplete.ResponseOutput
  type RequestOptions = CompilerServiceInterface.RequestOptions

  let respond = new PrintingAgent()

  let checker = FSharpChecker.Instance
  do checker.BeforeBackgroundFileCheck.Add (fun _ -> ())

  let mutable currentFiles = Map.empty
  let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let rec main (state:State) : int =
    currentFiles <- state.Files

    let parsed file =
      let ok = Map.containsKey file state.Files
      if not ok then respond.Error (sprintf "File '%s' not parsed" file)
      ok

    /// Is the specified position consistent with internal state of file?
    //  Note that both emacs and FCS use 1-based line indexing
    //  while emacs uses 0-based column indexing and FCS is 1-based
    let posok file line col =
      let lines = state.Files.[file].Lines
      let ok = line <= lines.Length && line >= 1 &&
               col <= lines.[line - 1].Length + 1 && col >= 1
      if not ok then respond.Error "Position is out of range"
      ok

    let getRequestOptions file state : RequestOptions =
      match Map.tryFind file state.FileCheckOptions with
      | Some x ->
          let text = String.concat "\n" state.Files.[file].Lines
          RequestOptions(x, file, text)
        
      | None ->

      RequestOptions(
          { ProjectFileName = file + ".fsproj"
            ProjectFileNames = [|file|]
            OtherOptions = [|"--noframework"|]
            ReferencedProjects = [| |]
            IsIncompleteTypeCheckEnvironment = true
            UseScriptResolutionRules = false   
            LoadTime = DateTime.Now //CompilerServiceInterface.fakeDateTimeRepresentingTimeLoaded file
            UnresolvedReferences = None },
          file,
          String.concat "\n" state.Files.[file].Lines)
      
    //Debug.print "main state is:\n %A\n%A"  (Map.toList state.Files)  (Map.toList state.Projects)
    match CommandInput.parseCommand(Console.ReadLine()) with
    | Parse(file,kind) ->
        let lines = readInput [] |> Array.ofList
        let file = Path.GetFullPath file

        let text = String.concat "\n" lines
        let state =
          { state with Files = state.Files |> Map.add file
                                                      { Lines = lines
                                                        Touched = DateTime.Now } }
        let options =
          if CompilerServiceInterface.isAScript file then
            let rawOptions = checker.GetProjectOptionsFromScript(file, text) |> Async.RunSynchronously
            let checkOptions = { rawOptions with OtherOptions = CompilerServiceInterface.ensureFSharpCore rawOptions.OtherOptions }
            RequestOptions(checkOptions,
                           file,
                           text)
          else
            getRequestOptions file state
        
        let task =
          async {
            let! _parseResults, checkResults = checker.ParseAndCheckFileInProject(options.FileName, 0, options.Source, options.Options)
            match checkResults with
            | FSharpCheckFileAnswer.Aborted -> ()
            | FSharpCheckFileAnswer.Succeeded results ->
                respond.WriteJson { Kind = "errors"
                                    Data = Seq.map FSharpErrorInfo.OfFSharpError results.Errors }
          }

        match kind with
        | Synchronous -> respond.Info "Synchronous parsing started"
                         Async.RunSynchronously task
        | Normal -> respond.Info "Background parsing started"
                    Async.StartImmediate task

        let state = { state with FileCheckOptions = state.FileCheckOptions
                                                    |> Map.add file options.Options }
        main state

    | Project file ->
        let file = Path.GetFullPath file
        if File.Exists file then
          let state =
            try
              let p = SourceCodeServices.FSharpProjectFileInfo.Parse(file)
              let files =
                [ for f in p.CompileFiles do
                    yield IO.Path.Combine(p.Directory, f) ]
              // TODO: Handle these options more gracefully
              let targetFilename = match p.OutputFile with Some p -> p | None -> "Unknown"
              let framework = match p.FrameworkVersion with Some p -> p | None -> "Unknown"
              let loadedTimeStamp = DateTime.Now //CompilerServiceInterface.fakeDateTimeRepresentingTimeLoaded file
              let projectOptions = checker.GetProjectOptionsFromCommandLineArgs(file, Array.ofList p.Options, loadedTimeStamp=loadedTimeStamp)
              let referencedProjectOptions =
                [| for file in p.ProjectReferences do
                       if Path.GetExtension(file) = ".fsproj" then
                           let opts = checker.GetProjectOptionsFromProjectFile(file, loadedTimeStamp=loadedTimeStamp)
                           yield file, opts |]

              respond.WriteJson
                  { Kind = "project"
                    Data = { Project = file
                             Files = files
                             Output = targetFilename
                             References = List.sortBy Path.GetFileName p.References
                             Framework = framework } }
              let po = { projectOptions
                         with ReferencedProjects = referencedProjectOptions }
              let projects =
                files
                |> List.fold (fun s f -> Map.add f po s) state.FileCheckOptions
              { state with FileCheckOptions = projects }
            with e ->
              respond.Error (sprintf "Project file '%s' is invalid: '%s'" file e.Message)
              state
          main state
        else
          respond.Error (sprintf "File '%s' does not exist" file)
          main state

    | Declarations file ->
        let file = Path.GetFullPath file
        if parsed file then
          let options = getRequestOptions file state
          let decls = CompilerServiceInterface.getDeclarations checker options
          respond.WriteJson { Kind = "declarations"; Data = decls }

        main state

    | HelpText sym ->

        match Map.tryFind sym state.HelpText with
        | None -> ()
        | Some d ->

          let tip = TipFormatter.formatTip d
          let helptext = Map.add sym tip Map.empty
          respond.WriteJson { Kind = "helptext"; Data = helptext }

        main state

    | PosCommand(cmd, file, line, col, timeout, filter) ->
        let file = Path.GetFullPath file
        if parsed file && posok file line col then
          let options = getRequestOptions file state
          let lineStr = state.Files.[file].Lines.[line - 1]
          let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(options.FileName, options.Options)
          match tyResOpt with
          | None -> respond.Error "Cached typecheck results not yet available"; main state
          | Some (parseResults, tyRes, _version) ->

          match cmd with
          | Completion ->
              let longName,residue = Parsing.findLongIdentsAndResidue(col - 1, lineStr)
              let results =
                try
                  let results =
                    Async.RunSynchronously (tyRes.GetDeclarationListInfo(Some parseResults, line, col, lineStr, longName, residue, fun (_,_) -> false),
                                            ?timeout = timeout)
                  Some (results, residue)
                with :? TimeoutException -> None
              match results with
              | Some (decls, residue) ->
                  let decls =
                    match filter with
                    | Some "StartsWith" ->  [| for d in decls.Items do if d.Name.StartsWith residue then yield d |]
                    | Some "Contains" -> [| for d in decls.Items do if d.Name.Contains residue then yield d |]
                    | _ -> decls.Items

                  let ds = Array.sortBy (fun (d: FSharpDeclarationListItem) -> d.Name) decls
                  match Array.tryFind (fun (d: FSharpDeclarationListItem) -> d.Name.StartsWith residue) ds with
                  | None -> ()
                  | Some d -> let tip = TipFormatter.formatTip d.DescriptionText
                              let helptext = Map.add d.Name tip Map.empty
                              respond.WriteJson { Kind = "helptext"; Data = helptext }

                  respond.WriteJson
                      { Kind = "completion"
                        Data = [ for d in decls do
                                   let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                                   yield { Name = d.Name; Glyph = glyph; GlyphChar = glyphChar } ] }

                  let helptext =
                    Seq.fold (fun m (d: FSharpDeclarationListItem) -> Map.add d.Name d.DescriptionText m) Map.empty decls

                  main { state with HelpText = helptext }
              | None ->
                  respond.Error "Could not get type information"
                  main state

          | ToolTip ->

              match Parsing.findLongIdents(col - 1, lineStr) with 
              | None -> respond.Info "Cannot find indent for tooltip"
              | Some(col,identIsland) ->

                // TODO: Display other tooltip types, for example for strings or comments where appropriate
                let tip = tyRes.GetToolTipTextAlternate(line, col + 1, lineStr, identIsland, Parser.tagOfToken(Parser.token.IDENT("")))
                          |> Async.RunSynchronously
                match tip with
                | FSharpToolTipText(elems) when elems |> List.forall (function
                  FSharpToolTipElement.None -> true | _ -> false) ->
                  respond.Info "No tooltip information"
                | _ -> respond.WriteJson { Kind = "tooltip"; Data = TipFormatter.formatTip tip }

              main state

          | SymbolUse ->
              let symboluses =
                  async {
                      match Parsing.findLongIdents(col - 1, lineStr) with 
                      | None -> return None
                      | Some(colu, identIsland) ->

                      let! symboluse = tyRes.GetSymbolUseAtLocation(line, colu + 1, lineStr, identIsland)
                      match symboluse with
                      | None -> return None
                      | Some symboluse ->

                      let! symboluses = tyRes.GetUsesOfSymbolInFile symboluse.Symbol
                      return Some {
                        Name = symboluse.Symbol.DisplayName
                        Uses =
                          [ for su in symboluses do
                              yield { StartLine = su.RangeAlternate.StartLine
                                      StartColumn = su.RangeAlternate.StartColumn + 1
                                      EndLine = su.RangeAlternate.EndLine
                                      EndColumn = su.RangeAlternate.EndColumn + 1
                                      Filename = su.FileName
                                      IsFromDefinition = su.IsFromDefinition
                                      IsFromAttribute = su.IsFromAttribute
                                      IsFromComputationExpression = su.IsFromComputationExpression
                                      IsFromDispatchSlotImplementation = su.IsFromDispatchSlotImplementation
                                      IsFromPattern = su.IsFromPattern
                                      IsFromType = su.IsFromType } ] } }
                  |> Async.RunSynchronously

              match symboluses with
              | Some su -> respond.WriteJson { Kind = "symboluse"; Data = su }
              | _ -> respond.Error "No symbols found"

              main state

          | FindDeclaration ->
            match Parsing.findLongIdents(col - 1, lineStr) with 
            | None -> respond.Info "Could not find ident at this location"
            | Some(col,identIsland) -> 

              let declarations = tyRes.GetDeclarationLocationAlternate(line, col + 1, lineStr, identIsland, false)
                                 |> Async.RunSynchronously
              match declarations with
              | FSharpFindDeclResult.DeclNotFound _ -> respond.Error "Could not find declaration"
              | FSharpFindDeclResult.DeclFound range ->

                    let data = { Line = range.StartLine; Column = range.StartColumn + 1; File = range.FileName }
                    respond.WriteJson { Kind = "finddecl"; Data = data }

            main state


          | Methods ->
            // Find the starting point, ideally right after the first '('
            let lineCutoff = line - 3
            let commas, line, col =
              let rec prevPos (line,col) =
                match line, col with
                | 1, 1
                | _ when line < lineCutoff -> 1, 1
                | _, 1 ->
                   let prevLine = state.Files.[file].Lines.[line - 2]
                   if prevLine.Length = 0 then prevPos(line-1, 1)
                   else line - 1, prevLine.Length
                | _    -> line, col - 1

              let rec loop commas depth (line, col) =
                if (line,col) <= (1,1) then (0, line, col) else
                let ch = state.Files.[file].Lines.[line - 1].[col - 1]
                let commas = if depth = 0 && ch = ',' then commas + 1 else commas
                if (ch = '(' || ch = '{' || ch = '[') && depth > 0 then loop commas (depth - 1) (prevPos (line,col))
                elif ch = ')' || ch = '}' || ch = ']' then loop commas (depth + 1) (prevPos (line,col))
                elif ch = '(' || ch = '<' then commas, line, col
                else loop commas depth (prevPos (line,col))
              match loop 0 0 (prevPos(line,col)) with
              | _, 1, 1 -> 0, line, col
              | newPos -> newPos

            let lineStr = state.Files.[file].Lines.[line - 1]
            match Parsing.findLongIdentsAtGetMethodsTrigger(col - 1, lineStr) with 
            | None -> respond.Info "Could not find ident at this location"
            | Some identIsland ->

              let meth = tyRes.GetMethodsAlternate(line, col, lineStr, Some identIsland)
                         |> Async.RunSynchronously
              respond.WriteJson
               { Kind = "method"
                 Data = { Name = meth.MethodName
                          CurrentParameter = commas
                          Overloads =
                           [ for o in meth.Methods do
                              let tip = TipFormatter.formatTip o.Description
                              yield {
                                Tip = tip
                                TypeText = o.TypeText
                                Parameters =
                                  [ for p in o.Parameters do
                                     yield {
                                       Name = p.ParameterName
                                       CanonicalTypeTextForSorting = p.CanonicalTypeTextForSorting
                                       Display = p.Display
                                       Description = p.Description
                                     }
                                ]
                                IsStaticArguments = o.IsStaticArguments
                              }

                           ] } }

            main state

        else
          main state

    | CompilerLocation ->
        let locopt = FSharpEnvironment.BinFolderOfDefaultFSharpCompiler None
        match locopt with
        | None -> respond.Error "Could not find compiler"
        | Some loc -> respond.WriteJson { Kind = "compilerlocation"; Data = loc }

        main state

    | Error(msg) ->
        respond.Error msg
        main state

    | Quit ->
        respond.Quit()
        (!Debug.output).Close ()
        0

  [<EntryPoint>]
  let entry args =
    // System.Diagnostics.Debug.Listeners.Add(
    //   new System.Diagnostics.TextWriterTraceListener(Console.Out))
    // |> ignore
    let extra = Options.p.Parse args
    if extra.Count <> 0 then
      printfn "Unrecognised arguments: %s" (String.concat "," extra)
      1
    else
      try
        main State.Initial
      finally
        (!Debug.output).Close ()
