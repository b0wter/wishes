namespace Wishes.Giraffe

open System
open System.Threading.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open FsToolkit.ErrorHandling

module HttpUtilities =
        /// Converts the result of a `taskResult` computational expression into a `HttpContext option`
        let mapErrorToResponse (ctx: HttpContext) (result: Task<Result<HttpContext option, string>>) : Task<HttpContext option> =
            task {
                match! result with
                | Ok o -> return o
                | Error e ->
                    ctx.SetStatusCode 500
                    return! ctx.WriteTextAsync e
            }
            
        let private handle
                (ctx: HttpContext)
                (next: HttpFunc)
                (validator: 'payload -> Validation<'model, string>)
                (errorHandler: int * string list -> HttpHandler)
                (successHandler: 'model -> HttpHandler)
                (input: 'payload) =
            try
                match input |> validator with
                | Validation.Ok v -> successHandler v next ctx
                | Validation.Error e -> errorHandler (400, e) next ctx
            with ex ->
                errorHandler (500, [ ex.Message ]) next ctx
            
        let private handleWithExtra
                (ctx: HttpContext)
                (next: HttpFunc)
                (validator: 'payload -> Validation<'model, string>)
                (errorHandler: int * string list -> HttpHandler)
                (successHandler: 'model -> 'extra -> HttpHandler)
                (extra: 'extra)
                (input: 'payload) =
            try
                match input |> validator with
                | Validation.Ok v -> successHandler v extra next ctx
                | Validation.Error e -> errorHandler (400, e) next ctx
            with ex ->
                errorHandler (500, [ ex.Message ]) next ctx
            
        let tryBindJson<'T> (errorHandler: int * string list -> HttpHandler) (validator: 'T -> Validation<'T, string>) (successHandler: 'T -> HttpHandler): HttpHandler =
            let inline isNullMatch value = obj.ReferenceEquals(value, null)
            fun (next : HttpFunc) (ctx : HttpContext) ->
                task {
                    try
                        let! model = ctx.BindJsonAsync<'T>()
                        if model |> isNullMatch then
                            return! errorHandler (400, [ "The request body is empty" ]) next ctx
                        else
                            return! handle ctx next validator errorHandler successHandler model
                    with ex ->
                        let errorMessage = sprintf $"Malformed request or missing field in request body, reason: %s{ex.Message}"
                        return! errorHandler (400, [ errorMessage ]) next ctx
                }
                
        let tryBindJsonWithExtra<'T, 'U> (errorHandler: int * string list -> HttpHandler) (validator: 'T -> Validation<'T, string>) (successHandler: 'T -> 'U -> HttpHandler) (extra: 'U): HttpHandler =
            let inline isNullMatch value = obj.ReferenceEquals(value, null)
            fun (next : HttpFunc) (ctx : HttpContext) ->
                task {
                    try
                        let! model = ctx.BindJsonAsync<'T>()
                        if model |> isNullMatch then
                            return! errorHandler (400, [ "The request body is empty" ]) next ctx
                        else
                            return! handleWithExtra ctx next validator errorHandler successHandler extra model
                    with ex ->
                        let errorMessage = sprintf $"Malformed request or missing field in request body, reason: %s{ex.Message}"
                        return! errorHandler (400, [ errorMessage ]) next ctx
                }
            
        let tryBindJsonAndTransform<'payload, 'entity> (errorHandler: int * string list -> HttpHandler) (validator: 'payload -> Validation<'entity, string>) (successHandler: 'entity -> HttpHandler): HttpHandler =
            let inline isNullMatch value = obj.ReferenceEquals(value, null)
            fun (next : HttpFunc) (ctx : HttpContext) ->
                task {
                    try
                        let! model = ctx.BindJsonAsync<'payload>()
                        if model |> isNullMatch then
                            return! errorHandler (400, [ "The request body is empty" ]) next ctx
                        else
                            return! handle ctx next validator errorHandler successHandler model
                    with ex ->
                        let errorMessages = [ sprintf $"Malformed request or missing field in request body, reason: %s{ex.Message}" ]
                        return! errorHandler (400, errorMessages) next ctx
                }
                