namespace Wishes.Repository

open System.Collections.Concurrent
open Microsoft.Extensions.Logging

module Repository =
    
    type private Dict<'key, 'value> = ConcurrentDictionary<'key, 'value>
    
    type InMemory<'key, 'value> private (dict: Dict<'key, 'value>, filename: string, logger: ILogger<InMemory<'key, 'value>>, customConverters: System.Text.Json.Serialization.JsonConverter list) =
        let mutable newChange = false
        let options = System.Text.Json.JsonSerializerOptions()
        do customConverters |> List.iter (fun c -> options.Converters.Add(c))
 
        let saveToFile filename =
            logger.LogInformation "Trying to save repository to file"
            try
                let serialized = System.Text.Json.JsonSerializer.Serialize(dict, options)
                System.IO.File.WriteAllText(filename, serialized)
                logger.LogInformation "Repository written to file"
            with
            | exn ->
                logger.LogCritical("Could not save repository to disk because", exn)
        
        member __.HasChangedSinceLastSave =
            newChange
        
        member __.AddOrUpdate (key: 'key, value: 'value) =
            let factory = fun _ -> fun _ -> value
            newChange <- true
            dict.AddOrUpdate(
                key,
                value,
                factory)
            
        /// <summary>
        /// Tries to remove a key value pair. If this does not succeed it is most likely
        /// </summary>
        /// <returns>
        /// `None` if the given key is unknown <br/>
        /// `Some true` if the key-value-pair was removed <br/>
        /// `Some false` if the key-value-pair could not be removed, this is likely due to a timing error
        /// </returns>
        member __.TryRemove key =
            if key |> dict.ContainsKey then
                let success, _ = dict.TryRemove key
                if success then
                    newChange <- true
                    Some success
                else
                    Some success
            else None
            
        member __.TryGetWishListById id =
            if id |> dict.ContainsKey then Some dict[id]
            else None

        member __.GetWishlists () =
            dict.Values |> List.ofSeq
        
        member __.Save () =
            saveToFile filename
            newChange <- false
            
        member __.SaveIfChanged () =
            if newChange then __.Save ()
            else ()

        member __.SaveToFile filename =
            saveToFile filename
            newChange <- false
            
        static member FromFile<'key, 'value> (filename, logger, customConverters) : InMemory<'key, 'value> =
            let options = System.Text.Json.JsonSerializerOptions()
            do customConverters |> List.iter (fun c -> options.Converters.Add(c))
            let lists =
                filename
                |> System.IO.File.ReadAllText
                |> (fun s -> System.Text.Json.JsonSerializer.Deserialize<Dict<'key, 'value>>(s, options))
            InMemory(lists, filename, logger, customConverters)
            
        static member Empty<'key, 'value> (filename, logger, customConverters) : InMemory<'key, 'value> =
            InMemory(Dict<'key, 'value>(), filename, logger, customConverters)
