namespace Wishes.Shared

open System

module Wishes =
    type Priority =
        | Low
        | Moderate
        | High
        | VeryHigh
    
    let tryParsePriority (s: string) =
        match s.ToLower() with
        | "low" -> Some Low
        | "moderate" -> Some Moderate
        | "high" -> Some High
        | "veryhigh" -> Some VeryHigh
        | _ -> None
        
    let priorityAsString p =
        match p with
        | Low -> "low"
        | Moderate -> "moderate"
        | High -> "high"
        | VeryHigh -> "veryhigh"
        
    type Wish = {
        Id: Guid
        Name: string
        Description: string option
        Urls: Uri list
        IsCompleted: bool
        CreationTime: DateTimeOffset
        Priority: Priority option
    }
    
    [<AbstractClass; Sealed>]
    type Factory private () =
        static member Create(title, ?description, ?url) =
            {
                Id = Guid.NewGuid ()
                Name = title
                Description = description
                Urls =
                    url
                    |> Option.map List.singleton
                    |> Option.defaultValue []
                IsCompleted = false
                CreationTime = DateTimeOffset.UtcNow
                Priority = None
            }

        static member Create(title, ?description, ?urls : Uri list) =
            {
                Id = Guid.NewGuid ()
                Name = title
                Description = description
                Urls =
                    urls
                    |> Option.defaultValue []
                IsCompleted = false
                CreationTime = DateTimeOffset.UtcNow
                Priority = None
            }

    type WishConverter() =
        inherit System.Text.Json.Serialization.JsonConverter<Wish>()

        override this.Read(reader, typeToConvert, options) = failwith "todo"
        override this.Write(writer, value, options) = failwith "todo"