namespace Wishes.Giraffe

open System
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Giraffe
open FsToolkit.ErrorHandling
open Wishes.Repository
open Wishes.Shared
open Wishes.Shared.Wishlists

module Wishlists =
    let private Errors =
        dict [
            ("wishlist_1", "A wishlist with the given id does not exist")
            ("wishlist_2", "The given wish does not exist on the wishlist")
            ("wishlist_3", "Could not update the wish because the wish ids from the route and body did not match")
        ]
        
    type RemoveWishlistTokenConverter() =
        inherit JsonConverter<Wishlist>()

        override this.Read(_, _, _) =
            failwith "This converter is not meant to read json"

        override this.Write(writer, wishlist, options) =
            let nonSensitive = wishlist |> asNonSensitive
            System.Text.Json.JsonSerializer.Serialize(writer, nonSensitive, options)
    
    type WishlistRepo = Repository.InMemory<Guid, Wishlist>
    
    module Validations =
        module Name =
            let isNotNullOrWhitespace (s: string) =
                if String.IsNullOrWhiteSpace(s) then Validation.error "The name of a wish list must not be empty"
                else Validation.ok s
                
            let doesNotExceedMaxLength (s: string) =
                if s.Length > 512 then Validation.error "A wish list name must not exceed 512 characters"
                else Validation.ok s
            
            let all = isNotNullOrWhitespace >> Validation.bind doesNotExceedMaxLength
                
        module Description =
            let isNotNullOrWhitespace (s: string) =
                if String.IsNullOrWhiteSpace(s) then Validation.error "The description of a wish list must not be empty if it is supplied"
                else Validation.ok s
            
            let doesNotExceedMaxLength (s: string) =
                if s.Length > 4096 then Validation.error "A wish list description must not exceed 4096 characters"
                else Validation.ok s
            
            let all = isNotNullOrWhitespace >> Validation.bind doesNotExceedMaxLength
    
        module Urls =
            let isValid url =
                try
                    let uri = Uri(url)
                    Validation.ok uri
                with
                | exn ->
                    Validation.error $"The url '%s{url}' could not be parsed because %s{exn.Message}"
            
    
    [<AbstractClass; Sealed>]
    type Factory private () =
        static member Create(name, ?description) =
            {
                Id = Guid.NewGuid ()
                Name = name
                Description = description
                Wishes = []
                Token = Utilities.generateToken 32
                CreationTime = DateTimeOffset.UtcNow
            }
        
        static member CreateWith(name, description) =
            {
                Id = Guid.NewGuid ()
                Name = name
                Description = description
                Wishes = []
                Token = Utilities.generateToken 32
                CreationTime = DateTimeOffset.UtcNow
            }
    
    module Show =
        let handler (id: Guid) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    match id |> repo.TryGetWishListById with
                    | Some wishlist ->
                        return! ctx.WriteJsonAsync { wishlist with Token = null }
                    | None ->
                        ctx.SetStatusCode 404
                        return! ctx.WriteJsonAsync
                                    {| WasRemoved = false
                                       Error = Errors["wishlist_1"]
                                       ErrorCode = "wishlist_1" |}
                } |> HttpUtilities.mapErrorToResponse ctx

    module ShowAll =
        let handler isDevelopment =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    if isDevelopment then
                        let repo = ctx.GetService<WishlistRepo>()
                        let all = repo.GetWishlists()
                        return! ctx.WriteJsonAsync all
                    else
                        do ctx.SetStatusCode 404
                        return! ctx.WriteStringAsync String.Empty
                } |> HttpUtilities.mapErrorToResponse ctx
            
    module ShowWish =
        let handler (listId: Guid, wishId: Guid) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    match listId |> repo.TryGetWishListById with
                    | Some wishlist ->
                        match wishlist.Wishes |> List.tryFind (fun w -> w.Id = wishId) with
                        | Some wish ->
                            return! ctx.WriteJsonAsync wish
                        | None ->
                            ctx.SetStatusCode 404
                            return! ctx.WriteJsonAsync
                                    {| Error = Errors["wishlist_2"]
                                       ErrorCode = "wishlist_2" |}
                    | None ->
                        ctx.SetStatusCode 404
                        return! ctx.WriteJsonAsync
                                {| Error = Errors["wishlist_1"]
                                   ErrorCode = "wishlist_1" |}
                } |> HttpUtilities.mapErrorToResponse ctx
            
    module New =
        type Payload = {
            Name: string
            Description: string option
        }
        
        let validateAndTransform (payload: Payload) =
            validation {
                let! validName =
                    payload.Name
                    |> Validations.Name.isNotNullOrWhitespace
                    |> Validation.bind Validations.Name.doesNotExceedMaxLength
                let! validDescription =
                    let validation =
                        Validations.Description.isNotNullOrWhitespace
                        >> Validation.bind Validations.Description.doesNotExceedMaxLength
                        >> Validation.map Some
                    payload.Description
                    |> Option.map validation
                    |> Option.defaultValue (Validation.ok None)
                return
                    Factory.CreateWith (validName, validDescription)
            }
        
        let handler (wishlist: Wishlist) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    do repo.AddOrUpdate (wishlist.Id, wishlist) |> ignore
                    return! ctx.WriteJsonAsync {| token = wishlist.Token; wishlist = wishlist |}
                } |> HttpUtilities.mapErrorToResponse ctx
                
    module UpdateWishlist =
        type Payload = {
            Name: string
            Description: string option
        }
        
        type WishlistUpdate = {
            Name: string
            Description: string option
        }
        
        let validateAndTransform (payload: Payload) =
            validation {
                let! validName =
                    payload.Name
                    |> Validations.Name.isNotNullOrWhitespace
                    |> Validation.bind Validations.Name.doesNotExceedMaxLength
                and! validDescription =
                    let validation =
                        Validations.Description.isNotNullOrWhitespace
                        >> Validation.bind Validations.Description.doesNotExceedMaxLength
                        >> Validation.map Some
                    payload.Description
                    |> Option.map validation
                    |> Option.defaultValue (Validation.ok None)
                return
                    {
                        WishlistUpdate.Name = validName
                        WishlistUpdate.Description = validDescription
                    }
            }
        
        let handler (update: WishlistUpdate) (oldWishlist: Wishlist) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    let updatedWishlist = { oldWishlist with Name = update.Name; Description = update.Description }
                    do repo.AddOrUpdate (updatedWishlist.Id, updatedWishlist) |> ignore
                    return! ctx.WriteJsonAsync updatedWishlist
                } |> HttpUtilities.mapErrorToResponse ctx
                

    module Delete =
        let handler (list: Wishlist) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    let wasRemoved = list.Id |> repo.TryRemove
                    match wasRemoved with
                    | Some result ->
                        return! ctx.WriteJsonAsync {| WasRemoved = result |}
                    | None ->
                        ctx.SetStatusCode 404
                        return! ctx.WriteJsonAsync
                                    {| WasRemoved = false
                                       Error = Errors["wishlist_1"]
                                       ErrorCode = "wishlist_1" |}
                } |> HttpUtilities.mapErrorToResponse ctx

    module AddWish =
        type Payload = {
            Name: string
            Description: string option
            Urls: string list option
        }

        let validateAndTransform (payload: Payload) =
            validation {
                let! validatedTitle = payload.Name |> Validations.Name.all
                and! validatedDesc =
                    match payload.Description with
                    | Some desc ->
                        desc
                        |> Validations.Description.all
                        |> Validation.map Some
                    | None -> Validation.ok None
                and! validatedUrls =
                    payload.Urls
                    |> Option.defaultValue []
                    |> List.map Validations.Urls.isValid
                    |> List.sequenceValidationA
                return
                    {
                        Wishes.Id = Guid.NewGuid()
                        Wishes.Description = validatedDesc
                        Wishes.Name = validatedTitle
                        Wishes.Urls = validatedUrls
                        Wishes.IsCompleted = false
                        Wishes.CreationTime = DateTimeOffset.Now
                        Wishes.Priority = None
                    }
            }
            
        let handler (wish: Wishes.Wish) (wishlist: Wishlist) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    let updatedList = { wishlist with Wishes = wish :: wishlist.Wishes }
                    do repo.AddOrUpdate (wishlist.Id, updatedList) |> ignore
                    return! ctx.WriteJsonAsync {| wishlist = updatedList; newWishId = wish.Id |}
                } |> HttpUtilities.mapErrorToResponse ctx

    module UpdateWish =
        type Payload = {
            Name: string
            Description: string option
            Urls: string list option
            Priority: string option
        }
        
        type WishUpdate = {
            Name: string
            Description: string option
            Urls: Uri list
            Priority: Wishes.Priority option
        }

        let validateAndTransform (payload: Payload) =
            validation {
                let! validatedTitle = payload.Name |> Validations.Name.all
                and! validatedDesc =
                    match payload.Description with
                    | Some desc ->
                        desc
                        |> Validations.Description.all
                        |> Validation.map Some
                    | None -> Validation.ok None
                and! validatedUrls =
                    payload.Urls
                    |> Option.defaultValue []
                    |> List.map Validations.Urls.isValid
                    |> List.sequenceValidationA
                and! validatedPriority =
                    match payload.Priority with
                    | Some p ->
                        match p |> Wishes.tryParsePriority with
                        | Some parsed -> Validation.ok (Some parsed)
                        | None -> Validation.error "The given priority could not be parsed"
                    | None -> Validation.ok None
                return
                    {
                        WishUpdate.Description = validatedDesc
                        WishUpdate.Name = validatedTitle
                        WishUpdate.Urls = validatedUrls
                        WishUpdate.Priority = validatedPriority
                    }
            }
        
        let handler (wishId: Guid) (update: WishUpdate) (wishlist: Wishlist) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    match wishlist.Wishes |> List.tryFind (fun w -> w.Id = wishId) with
                    | Some wish ->
                        let updatedWish = { wish with Name = update.Name; Description = update.Description; Urls = update.Urls; Priority = update.Priority }
                        let repo = ctx.GetService<WishlistRepo>()
                        let updatedList = updatedWish |> updateWishIn wishlist
                        do repo.AddOrUpdate (wishlist.Id, updatedList) |> ignore
                        return! ctx.WriteJsonAsync updatedList
                    | None ->
                        do ctx.SetStatusCode 404
                        return! ctx.WriteJsonAsync {| Error = Errors["wishlist_2"]
                                                      ErrorCode = "wishlist_2" |}
                } |> HttpUtilities.mapErrorToResponse ctx
            
    module DeleteWish =
        let handler (wishId: Guid) (list: Wishlist) =
            fun (_: HttpFunc) (ctx: HttpContext) ->
                taskResult {
                    let repo = ctx.GetService<WishlistRepo>()
                    let doesWishExist = list.Wishes |> List.exists (fun w -> w.Id = wishId)
                    if doesWishExist then
                        let updatedList = list |> removeWishByIdFrom wishId
                        let persistedList = (updatedList.Id, updatedList) |> repo.AddOrUpdate
                        return! persistedList |> ctx.WriteJsonAsync
                    else
                        ctx.SetStatusCode 404
                        return! ctx.WriteJsonAsync {| Error = Errors["wishlist_2"]
                                                      ErrorCode = "wishlist_2" |}
                } |> HttpUtilities.mapErrorToResponse ctx

    module MarkWishAs =
        type MarkAs
            = IsCompleted
            | IsNotCompleted

        let markAs (ctx: HttpContext) (listId: Guid) (wishId: Guid) (markAs: MarkAs) =
            let repo = ctx.GetService<WishlistRepo>()
            
            let maybeList = listId |> repo.TryGetWishListById
            let maybeWish = maybeList |> Option.bind (findWish wishId)
                
            match maybeList, maybeWish with
            | Some list, Some wish ->
                let completed =
                    match markAs with
                    | IsCompleted -> true
                    | IsNotCompleted -> false
                let updatedWish = { wish with IsCompleted = completed }
                let updatedList = updatedWish |> updateWishIn list
                let persisted = repo.AddOrUpdate (listId, updatedList)
                ctx.WriteJsonAsync persisted
            | Some _, None ->
                ctx.SetStatusCode 404
                ctx.WriteJsonAsync {| Error = Errors["wishlist_2"]
                                              ErrorCode = "wishlist_2" |}
            | None, _ ->
                ctx.SetStatusCode 404
                ctx.WriteJsonAsync {| Error = Errors["wishlist_1"]
                                              ErrorCode = "wishlist_1" |}
    
        module Completed =
            let handler (listId: Guid, wishId: Guid) =
                fun (_: HttpFunc) (ctx: HttpContext) ->
                    taskResult {
                        return! markAs ctx listId wishId MarkAs.IsCompleted
                    } |> HttpUtilities.mapErrorToResponse ctx

        module NotCompleted =
            let handler (listId: Guid, wishId: Guid) =
                fun (_: HttpFunc) (ctx: HttpContext) ->
                    taskResult {
                        return! markAs ctx listId wishId MarkAs.IsNotCompleted
                    } |> HttpUtilities.mapErrorToResponse ctx
