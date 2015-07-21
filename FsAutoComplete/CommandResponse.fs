namespace FsAutoComplete

open System
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Microsoft.FSharp.Compiler

module ResponseOutput =

  type ResponseMsg<'T> =
    {
      Kind: string
      Data: 'T
    }

  type Location =
    {
      File: string
      Line: int
      Column: int
    }

  type CompletionResponse =
    {
      Name: string
      Glyph: string
      GlyphChar: string
    }

  type ProjectResponse =
    {
      Project: string
      Files: List<string>
      Output: string
      References: List<string>
      Framework: string
    }

  type OverloadParameter =
    {
      Name : string
      CanonicalTypeTextForSorting : string
      Display : string
      Description : string
    }
  type Overload =
    {
      Tip : string
      TypeText : string
      Parameters : OverloadParameter list
      IsStaticArguments : bool
    }
  type MethodResponse =
    {
      Name : string
      CurrentParameter : int
      Overloads : Overload list
    }

  type SymbolUseRange =
    {
      Filename: string
      StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      IsFromDefinition: bool
      IsFromAttribute : bool
      IsFromComputationExpression : bool
      IsFromDispatchSlotImplementation : bool
      IsFromPattern : bool
      IsFromType : bool
    }

  type SymbolUseResponse =
    {
      Name: string
      Uses: SymbolUseRange list
    }

  type FSharpErrorInfo =
    {
      FileName: string
      StartLine:int
      EndLine:int
      StartColumn:int
      EndColumn:int
      Severity:FSharpErrorSeverity
      Message:string
      Subcategory:string
    }
    static member OfFSharpError(e:Microsoft.FSharp.Compiler.FSharpErrorInfo) =
      {
        FileName = e.FileName
        StartLine = e.StartLineAlternate
        EndLine = e.EndLineAlternate
        StartColumn = e.StartColumn + 1
        EndColumn = e.EndColumn + 1
        Severity = e.Severity
        Message = e.Message
        Subcategory = e.Subcategory
      }

  type FSharpErrorSeverityConverter() =
    inherit JsonConverter()

    override x.CanConvert(t:System.Type) = t = typeof<FSharpErrorSeverity>

    override x.WriteJson(writer, value, serializer) =
      match value :?> FSharpErrorSeverity with
      | FSharpErrorSeverity.Error -> serializer.Serialize(writer, "Error")
      | FSharpErrorSeverity.Warning -> serializer.Serialize(writer, "Warning")

    override x.ReadJson(_reader, _t, _, _serializer) =
      raise (System.NotSupportedException())

    override x.CanRead = false
    override x.CanWrite = true

  type RangeConverter() =
    inherit JsonConverter()

    override x.CanConvert(t:System.Type) = t = typeof<Range.range>

    override x.WriteJson(writer, value, _serializer) =
      let range = value :?> Range.range
      writer.WriteStartObject()
      writer.WritePropertyName("StartColumn")
      writer.WriteValue(range.StartColumn + 1)
      writer.WritePropertyName("StartLine")
      writer.WriteValue(range.StartLine)
      writer.WritePropertyName("EndColumn")
      writer.WriteValue(range.EndColumn + 1)
      writer.WritePropertyName("EndLine")
      writer.WriteValue(range.EndLine)
      writer.WriteEndObject()

    override x.ReadJson(_reader, _t, _, _serializer) =
      raise (System.NotSupportedException())

    override x.CanRead = false
    override x.CanWrite = true

  type internal PrintingAgent() =
    let agent = MailboxProcessor.Start(fun agent ->
      let rec loop () = async {
          let! (msg: Choice<string,AsyncReplyChannel<unit>>) = agent.Receive()
          match msg with
          | Choice1Of2 (s: string) -> Console.WriteLine s; return! loop ()
          | Choice2Of2 ch -> ch.Reply ()
        }
      loop ()
      )

    let jsonConverters =
      [|
       new FSharpErrorSeverityConverter() :> JsonConverter;
       new RangeConverter() :> JsonConverter
      |]

    member private x.Write(s) = agent.Post (Choice1Of2 s)

    member x.WriteJson(o: obj) = x.Write (JsonConvert.SerializeObject(o, jsonConverters))

    member x.Info(s) = x.WriteJson { Kind = "info"; Data = s }
    member x.Error(s) = x.WriteJson { Kind = "error"; Data = s }

    member x.Quit() = agent.PostAndReply(fun ch -> Choice2Of2 ch)

