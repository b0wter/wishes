namespace Wishes.Giraffe

open System

module Wishes =
    type Wish = {
        Id: Guid
        Title: string
        Description: string option
        Urls: Uri list
        IsCompleted: bool
    }
    
    [<AbstractClass; Sealed>]
    type Factory private () =
        static member Create(title, ?description, ?url) =
            {
                Id = Guid.NewGuid ()
                Title = title
                Description = description
                Urls =
                    url
                    |> Option.map List.singleton
                    |> Option.defaultValue []
                IsCompleted = false
            }
