FROM mcr.microsoft.com/dotnet/sdk:6.0-focal AS build
WORKDIR /app
ARG ENVIRONMENT=Development
COPY . ./
WORKDIR /app/giraffe
RUN dotnet publish -c Release -o out --no-cache

FROM mcr.microsoft.com/dotnet/aspnet:6.0-focal AS runtime
WORKDIR /app
COPY --from=build /app/giraffe/out .
ARG ENVIRONMENT=Development
ENTRYPOINT [ "./Wishes.Giraffe.App"]
