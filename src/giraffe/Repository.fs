namespace Wishes.Giraffe

open System
open System.Collections.Concurrent
open Microsoft.Extensions.Logging

module Repository =
    
    type private Dict<'key, 'value> = ConcurrentDictionary<'key, 'value>
    
    type InMemory<'key, 'value> private (dict: Dict<'key, 'value>, filename: string, logger: ILogger) =
        let mutable lastSavedAt = DateTime.Now
        let saveIntervall = TimeSpan.FromMinutes(1.0)

        let saveToFile filename =
            logger.LogInformation "Trying to save repository to file"
            let serialized = dict |> System.Text.Json.JsonSerializer.Serialize
            System.IO.File.WriteAllText(filename, serialized)
            logger.LogInformation "Repository written to file"
            
        let saveIfSaveIntervalPassed () =
            let timePassed = DateTime.Now - lastSavedAt
            if timePassed > saveIntervall then
                lastSavedAt <- DateTime.Now
                saveToFile filename
            else ()
        
        member __.AddOrUpdate (key: 'key, value: 'value) =
            let factory = fun _ -> fun _ -> value
            let value = dict.AddOrUpdate(
                key,
                value,
                factory)
            do saveIfSaveIntervalPassed ()
            value
            
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
                Some success
            else None
            
        member __.TryGetWishListById id =
            if id |> dict.ContainsKey then Some dict[id]
            else None

        member __.GetWishlists () =
            dict.Values |> List.ofSeq
        
        member __.Save () =
            saveToFile filename

        member __.SaveToFile filename =
            saveToFile filename
            
        static member FromFile<'key, 'value> (filename, logger) : InMemory<'key, 'value> =
            let lists =
                filename
                |> System.IO.File.ReadAllText
                |> System.Text.Json.JsonSerializer.Deserialize<Dict<'key, 'value>>
            InMemory(lists, filename, logger)
            
        static member Empty<'key, 'value> (filename, logger) : InMemory<'key, 'value> =
            InMemory(Dict<'key, 'value>(), filename, logger)
