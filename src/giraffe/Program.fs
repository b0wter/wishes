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

let wishlistFile =
    let fromEnv = Environment.GetEnvironmentVariable("WISHES_FILENAME")
    if fromEnv |> String.IsNullOrWhiteSpace then
        "wishlists.json"
    else
        fromEnv

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
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
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
        app.UseDeveloperExceptionPage()
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    let addCustomJsonHandling (s: IServiceCollection) =
        let serializationOptions = SystemTextJson.Serializer.DefaultOptions
        serializationOptions.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
        serializationOptions.Converters.Add(Json.ListConverter())
        serializationOptions.Converters.Add(Json.MapConverter())
        serializationOptions.Converters.Add(Json.OptionConverter())
        serializationOptions.Converters.Add(Json.ListValueConverter())
        serializationOptions.Converters.Add(Json.MapValueConverter())
        serializationOptions.Converters.Add(Json.OptionValueConverter())
        services.AddSingleton<Json.ISerializer>(SystemTextJson.Serializer(serializationOptions)) |> ignore
        s
        
    let createRepository (s: IServiceProvider) =
        let logger = s.GetService<ILogger<Repository.InMemory<Guid, Wishlists.Wishlist>>>()
        if wishlistFile |> File.Exists then
            Repository.InMemory.FromFile<Guid, Wishlists.Wishlist>(wishlistFile, logger)
        else
            Repository.InMemory.Empty<Guid, Wishlists.Wishlist>(wishlistFile, logger)
        
    services.AddCors()    |> ignore
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