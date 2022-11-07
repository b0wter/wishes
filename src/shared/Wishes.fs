namespace Wishes.Shared

open System

module Wishes =
    type Wish = {
        Id: Guid
        Name: string
        Description: string option
        Urls: Uri list
        IsCompleted: bool
        CreationTime: DateTimeOffset
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
            }
