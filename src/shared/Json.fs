namespace Wishes.Shared

open System
open System.Text.Json
open System.Text.Json.Serialization
open Wishes.Shared.Wishes
open System.Linq

module Json =
    
    let private keys =
        [ ("low", Priority.Low)
          ("moderate", Priority.Moderate)
          ("high", Priority.High)
          ("veryhigh", Priority.VeryHigh)
        ] |> dict
    
    type PriorityConverter() =
        inherit JsonConverter<Priority>()

        override this.Read (reader: byref<Utf8JsonReader>, _, _: JsonSerializerOptions) =
            match reader.TokenType with
            | JsonTokenType.Null -> failwith "Cannot deserialize priority from null"
            | JsonTokenType.String ->
                match reader.GetString() with
                | s when String.IsNullOrWhiteSpace(s) -> failwith "Cannot deserialize priority from empty string"
                | s when keys.ContainsKey(s) -> keys[s]
                | s -> failwith $"Value '%s{s}' is an unknown priority"
            | _ -> failwith "Priorities can only be read from string values"

        override this.Write (writer: Utf8JsonWriter, value: Priority, _: JsonSerializerOptions) =
            match keys.Where(fun k -> k.Value = value) |> List.ofSeq with
            | [] -> failwith $"Cannot serialize priority '%A{value}', it is unknown"
            | [ single ] ->
                writer.WriteStringValue(single.Key)
            | many -> failwith $"Found too many possibilities to serialize the priority %A{value}: %A{many}"

    type PriorityOptionConverter() =
        inherit JsonConverter<Priority option>()
        let converter = PriorityConverter()

        override this.Read(reader, _, options) =
            match reader.TokenType with
            | JsonTokenType.Null -> None
            | JsonTokenType.String ->
                JsonSerializer.Deserialize<Priority>(&reader, options) |> Some
            | _ -> failwith "Priorities can only be read from string values"
            
        override this.Write(writer, value, options) =
            match value with
            | Some p -> converter.Write(writer, p, options)
            | None -> JsonSerializer.Serialize(writer, null, options)
        
        