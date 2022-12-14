module Wishes.Giraffe.App

open System
open System.IO
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Wishes.Repository
open Wishes.Shared

let wishlistFile =
    let fromEnv = Environment.GetEnvironmentVariable("WISHES_FILENAME")
    if fromEnv |> String.IsNullOrWhiteSpace then
        "wishlists.json"
    else
        fromEnv

let corsOrigins =
    let fromEnv = Environment.GetEnvironmentVariable("WISHES_ORIGINS")
    if fromEnv |> String.IsNullOrWhiteSpace then
        [|
            "http://localhost:5000"
            "https://localhost:5001"
            "http://localhost:8081"
            "https://wishes.plugman.de"
        |]
    else
        fromEnv.Split("%%")

let mutable isDevelopment = false

let defaultErrorHandler (statusCode: int, reasons: string list) : HttpHandler =
    setStatusCode statusCode >=> json {| Errors = reasons |}

// ---------------------------------
// Web app
// ---------------------------------
let jsonParsingError message =
    setStatusCode 400 >=> json {| Errors = message |}
    
let defaultBindJson<'a> =
    HttpUtilities.tryBindJson<'a> defaultErrorHandler

let defaultBindJsonWithArg<'a, 'b> =
    HttpUtilities.tryBindJsonWithExtra<'a, 'b> defaultErrorHandler

let defaultBindJsonAndValidate<'a, 'b> =
    HttpUtilities.tryBindJsonAndTransform<'a, 'b> defaultErrorHandler
    
let defaultBindJsonAndTransformWithExtra<'a, 'b, 'c> =
    HttpUtilities.tryBindJsonAndTransformWithExtra<'a, 'b, 'c> defaultErrorHandler

let mustHaveToken wishlistId =
    let tryGetWishlist (ctx: HttpContext) =
        let repo = ctx.GetService<Wishlists.WishlistRepo>()
        wishlistId |> repo.TryGetWishListById
        
    let validator (token: string) (wishlist: Wishlists.Wishlist) : bool =
        wishlist.Token = token
        
    HttpUtilities.mustHaveToken "token" Some tryGetWishlist validator

let webApp =
    choose [
        GET >=>
            choose [
                // following route uses verbose syntax because `isDevelopment` needs to be evaluated at call time
                route  "/wishlists"                 >=> fun next ctx -> Wishlists.ShowAll.handler isDevelopment next ctx
                routef "/wishlists/%O"              (fun id      -> Wishlists.Show.handler id)
                routef "/wishlists/%O/%O"           (fun idTuple -> Wishlists.ShowWish.handler idTuple)
            ]
        PATCH >=>
            choose [
                routef "/wishlists/%O/%O/complete"  (fun idTuple -> Wishlists.MarkWishAs.Completed.handler idTuple)
                routef "/wishlists/%O/%O/uncomplete"(fun idTuple -> Wishlists.MarkWishAs.NotCompleted.handler idTuple)
            ]
        PUT >=>
            choose [
                routef "/wishlists/%O"              (fun id -> mustHaveToken id (defaultBindJsonAndTransformWithExtra Wishlists.UpdateWishlist.validateAndTransform Wishlists.UpdateWishlist.handler))
                routef "/wishlists/%O/%O"           (fun (listId, wishId) -> mustHaveToken listId (defaultBindJsonAndTransformWithExtra Wishlists.UpdateWish.validateAndTransform (Wishlists.UpdateWish.handler wishId)))
            ]
        POST >=>
            choose [
                route  "/wishlists"                 >=> defaultBindJsonAndValidate Wishlists.New.validateAndTransform Wishlists.New.handler
                routef "/wishlists/%O/addwish"      (fun id -> mustHaveToken id (defaultBindJsonAndTransformWithExtra Wishlists.AddWish.validateAndTransform Wishlists.AddWish.handler))
            ]
        DELETE >=>
            choose [
                routef "/wishlists/%O"              (fun id -> mustHaveToken id Wishlists.Delete.handler)
                routef "/wishlists/%O/%O"           (fun (listId, wishId) -> mustHaveToken listId (Wishlists.DeleteWish.handler wishId))
            ]
        setStatusCode 404                           >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
       .AllowAnyOrigin()
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let onShutdown (services: IServiceProvider) =
    let repo = services.GetService<Repository.InMemory<Guid, Wishlists.Wishlist>>()
    repo.Save ()

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    let lifetime = app.ApplicationServices.GetService<IHostApplicationLifetime>()
    do lifetime.ApplicationStopping.Register(fun () -> onShutdown app.ApplicationServices) |> ignore
    do isDevelopment <- env.IsDevelopment()
    (match env.IsDevelopment() with
    | true  ->
        app
         .UseDeveloperExceptionPage()
    | false ->
        app
         .UseGiraffeErrorHandler(errorHandler)
         .UseHttpsRedirection())
         .UseStaticFiles()
         .UseCors("_defaultCorsPolicy")
         .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    let addCustomJsonHandling (s: IServiceCollection) =
        let serializationOptions = SystemTextJson.Serializer.DefaultOptions
        serializationOptions.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
        serializationOptions.Converters.Add(Wishlists.RemoveWishlistTokenConverter())
        serializationOptions.Converters.Add(Json.PriorityOptionConverter())
        serializationOptions.Converters.Add(Json.PriorityConverter())
        serializationOptions.Converters.Add(JsonFSharpConverter())
        services.AddSingleton<Json.ISerializer>(SystemTextJson.Serializer(serializationOptions)) |> ignore
        s

    let createRepository (s: IServiceProvider) =
        let logger = s.GetService<ILogger<Repository.InMemory<Guid, Wishlists.Wishlist>>>()
        if wishlistFile |> File.Exists then
            Repository.InMemory.FromFile<Guid, Wishlists.Wishlist>(wishlistFile, logger, [ JsonFSharpConverter(); ])
        else
            Repository.InMemory.Empty<Guid, Wishlists.Wishlist>(wishlistFile, logger, [ JsonFSharpConverter(); ])

    services.AddCors(fun options ->
            options.AddPolicy( "_defaultCorsPolicy", fun policy ->
                    do printfn "Allowing CORS for all headers/methods/origins"
                    policy.AllowAnyHeader().AllowAnyMethod().AllowAnyOrigin() |> ignore
                )
        ) |> ignore
    services.AddGiraffe() |> ignore
    services |> addCustomJsonHandling |> ignore
    services.AddSingleton<Repository.InMemory<Guid, Wishlists.Wishlist>>(createRepository) |> ignore
    services.AddHostedService<Services.RepositorySaveService<Guid, Wishlists.Wishlist>>() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0