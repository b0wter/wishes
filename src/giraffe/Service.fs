namespace Wishes.Giraffe

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

module Services =
    [<AbstractClass>]
    type TimedHostedService(logger: ILogger<TimedHostedService>) =
        let interval = TimeSpan.FromSeconds(5)

        abstract member DoWork: obj -> unit

        member __.Timer = new Timer(__.DoWork, null, Timeout.InfiniteTimeSpan, TimeSpan.FromSeconds(5))

        interface IHostedService with
            member __.StartAsync _ =
                do logger.LogInformation $"Starting %s{__.GetType().Name}"
                do __.Timer.Change(TimeSpan.Zero, interval) |> ignore
                Task.CompletedTask
                
            member __.StopAsync _ =
                do logger.LogInformation $"Stopping %s{__.GetType().Name}"
                do __.Timer.Change(Timeout.InfiniteTimeSpan, interval) |> ignore
                Task.CompletedTask
            
        interface IDisposable with
            member __.Dispose() =
                __.Timer.Dispose()


    type RepositorySaveService<'key, 'value>(repository: Wishes.Repository.Repository.InMemory<'key, 'value>, logger: ILogger<TimedHostedService>) =
        inherit TimedHostedService(logger)

        override this.DoWork _ =
            if repository.HasChangedSinceLastSave then
                do logger.LogInformation "Repository has changed since last save"
                repository.Save ()
            else
                do logger.LogInformation "Repository has not changed since last save"