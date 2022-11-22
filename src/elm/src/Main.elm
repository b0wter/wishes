port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input exposing (onInput)

import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Dict
import Html exposing (Html, div, h1, h4, small, text)
import Html.Attributes exposing (class, disabled, for, href, style)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Iso8601
import Json.Encode
import Maybe.Extra as Maybe
import Task
import Time
import UUID exposing (UUID)
import Url exposing (Protocol(..), Url)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import QS as QS
import Url.Parser exposing (Parser, (</>), map, oneOf, s)
import Json.Decode exposing (Decoder, bool, field, list, maybe, string)
import Duration as Duration
import Round 
import List.Extra as List


port copy : String -> Cmd msg

port receiveCopyResult : (Bool -> msg) -> Sub msg


apiUrl : String
apiUrl = "https://wishes-api.plugman.de"


-- MAIN

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = mainView
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveCopyResult CopyResult
        ]


-- MISC

uuidParser : Parser (UUID -> a) a
uuidParser =
    Url.Parser.custom "UUID" <| \segment ->
        case segment |> UUID.fromString of   
            Ok foo -> Just foo
            Err _ -> Nothing


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
      [ map WelcomeRoute (s "wishlists")
      , map WishlistRoute (s "wishlists" </> uuidParser)
      , map WelcomeRoute (Url.Parser.top)
      ]


{-| Tries to get a string from the query parameter values -}
tryFromQuery : String -> Url -> Maybe String
tryFromQuery key url =
    let
        dict =
            url.query |> Maybe.map (QS.parse QS.config)

        val =
            dict |> Maybe.andThen (Dict.get key)
    in
    case val of
        Just (QS.One o) ->
            Just o

        Just (QS.Many (head :: _)) ->
            Just head

        Just (QS.Many []) ->
            Nothing

        Nothing ->
            Nothing


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus 403 ->
            "You are not allowed to do this"
        BadStatus i ->
            "Unknown error, status code " ++ (i |> String.fromInt)
        BadBody errorMessage ->
            errorMessage


requestWishlistFromApi : String -> UUID -> Cmd Msg
requestWishlistFromApi url id =
    Http.get
      { url = url ++ "/wishlists/" ++ (id |> UUID.toString)
      , expect = Http.expectJson (\r -> (MessageForLoadingWishlist (GotWishlistFromApi r))) wishlistDecoder
      }
      
      
{-| Sends the minimum amount of fields required to create a new wishlist on the backend side -}
registerNewWishlistFromApi : String ->  NewWishlistRequest -> Cmd Msg
registerNewWishlistFromApi url wishlist =
    let payload = wishlist |> newWishlistEncoder
    in
    Http.post
      { url = url
      , body = Http.jsonBody payload
      , expect = Http.expectJson (\r -> (MessageForWelcome (GotNewWishlist r))) newWishlistDecoder
      }
      
      
deleteWishFromApi : UUID -> UUID -> String -> Cmd Msg
deleteWishFromApi wishlistId wishId authToken =
    let
        wishlist = wishlistId |> UUID.toString
        wish = wishId |> UUID.toString
    in
    Http.request
        { method = "DELETE"
        , headers = []
        , body = Http.emptyBody
        , url = apiUrl ++ "/wishlists/" ++ wishlist ++ "/" ++ wish ++ "?token=" ++ authToken
        , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist (r, False)))) wishlistDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


addWishToApi : UUID -> String -> NewWish -> Cmd Msg
addWishToApi wishlistId authToken newWish =
    let
        wishlist = (wishlistId |> UUID.toString)
        json = newWish |> newWishEncoder
    in
    Http.post
    { url = apiUrl ++ "/wishlists/" ++ wishlist ++ "/addwish?token=" ++ authToken
    , body = Http.jsonBody json
    , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist (r, True)))) addWishToWishlistDecoder
    }


updateWishInApi : UUID -> String -> Wish -> Cmd Msg
updateWishInApi wishlistUuid authToken wish =
    let
        wishlistId = wishlistUuid |> UUID.toString
        wishId = wish.id |> UUID.toString
        json = wish |> wishEncoder
    in
    Http.request
    { method = "PUT"
    , headers = []
    , url = apiUrl ++ "/wishlists/" ++ wishlistId ++ "/" ++ wishId ++ "?token=" ++ authToken
    , body = Http.jsonBody json
    , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist (r, True)))) wishlistDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


markWishAsCompleted : UUID -> UUID -> Cmd Msg      
markWishAsCompleted =
    markWishAs True
    
    
markWishAsNotCompleted : UUID -> UUID -> Cmd Msg      
markWishAsNotCompleted =
    markWishAs False
      
      
markWishAs : Bool -> UUID -> UUID -> Cmd Msg      
markWishAs isCompleted wishlistId wishId =
    let
        wishlist = wishlistId |> UUID.toString
        wish = wishId |> UUID.toString
        state = if isCompleted then "complete" else "uncomplete"
    in
    Http.request
      { method = "PATCH"
      , headers = []
      , body = Http.emptyBody
      , url = apiUrl ++ "/wishlists/" ++ wishlist ++ "/" ++ wish ++ "/" ++ state
      , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist (r, False)))) wishlistDecoder
      , timeout = Nothing
      , tracker = Nothing
      }
      
      
newWishlistEncoder : NewWishlistRequest -> Json.Encode.Value
newWishlistEncoder wishlist =
    let 
        description =
            case wishlist.description of
                Just s  -> s |> Json.Encode.string
                Nothing -> Json.Encode.null
    in
    Json.Encode.object
        [ ("name", Json.Encode.string wishlist.name)
        , ("description", description)
        ]


newWishEncoder : NewWish -> Json.Encode.Value
newWishEncoder newWish =
    let
        description =
            if newWish.description |> String.isEmpty then Json.Encode.null
            else newWish.description |> Json.Encode.string
        urls =
            if newWish.url |> String.isEmpty then Json.Encode.null
            else [ newWish.url ] |> (Json.Encode.list Json.Encode.string)
    in
    Json.Encode.object
        [ ("name", Json.Encode.string newWish.name)
        , ("description", description)
        , ("urls", urls)
        ]
    
    
wishEncoder : Wish -> Json.Encode.Value
wishEncoder wish =
    let
        description =
            case wish.description of
                Just "" -> Json.Encode.null
                Nothing -> Json.Encode.null
                Just d -> Json.Encode.string d
        priority =
            case wish.priority of
                Just Low -> Json.Encode.string "low"
                Just Moderate -> Json.Encode.string "moderate"
                Just High -> Json.Encode.string "high"
                Just VeryHigh -> Json.Encode.string "veryhigh"
                Nothing -> Json.Encode.null
    in
    Json.Encode.object
        [ ("name", Json.Encode.string wish.name)
        , ("description", description)
        , ("priority", priority)
        , ("urls", Json.Encode.list Json.Encode.string wish.urls)
        ]
    

newWishlistDecoder : Decoder NewWishlist
newWishlistDecoder =
    Json.Decode.map2 NewWishlist
        (field "token" string)
        (field "wishlist" wishlistDecoder)
      
      
wishlistDecoder : Decoder Wishlist
wishlistDecoder =
    Json.Decode.map5 Wishlist
        (field "id" UUID.jsonDecoder)
        (field "name" string)
        (maybe (field "description" Json.Decode.string))
        (field "wishes" wishesDecoder)
        (field "creationTime" Iso8601.decoder)
        
        
addWishToWishlistDecoder : Decoder Wishlist
addWishToWishlistDecoder =
    field "wishlist" wishlistDecoder
        

wishesDecoder : Decoder (List Wish)
wishesDecoder =
    list wishDecoder


priorityDecoder : Decoder Priority
priorityDecoder =
    Json.Decode.string
        |> Json.Decode.andThen (\str -> 
            case str of
                "low" -> Json.Decode.succeed  Low
                "moderate" -> Json.Decode.succeed Moderate
                "high" -> Json.Decode.succeed High
                "veryhigh" -> Json.Decode.succeed VeryHigh
                _ -> Json.Decode.fail "unknown priority"
        )

    
wishDecoder : Decoder Wish
wishDecoder =
    Json.Decode.map7 Wish
        (field "id" UUID.jsonDecoder)
        (field "name" string)
        (maybe (field "description" string))
        (field "urls" (list string))
        (field "isCompleted" bool)
        (field "creationTime" Iso8601.decoder)
        (maybe (field "priority" priorityDecoder))

-- MODEL

type PageState
    = NotFoundState
    | LoadingWishlistState LoadingWishlistModel
    | WishlistLoadedState WishlistLoadedModel
    | WelcomeState WelcomeModel
    | ErrorState (String, String)
    
type Route
    = WishlistRoute UUID
    | WelcomeRoute
    | NotFoundRoute
    
type Priority
    = Low
    | Moderate
    | High
    | VeryHigh

type alias Wish =
    { id: UUID
    , name: String
    , description: Maybe String
    , urls: List String
    , isCompleted: Bool
    , creationTime: Time.Posix
    , priority: Maybe Priority
    }
    
type alias NewWish =
    { name: String
    , description: String
    , url: String
    }

type alias Wishlist =
    { id: UUID
    , name: String
    , description: Maybe String
    , wishes: List Wish
    , creationTime: Time.Posix
    }
    
type alias NewWishlist =
    { token: String
    , wishlist: Wishlist
    }
    
type alias NewWishlistRequest =
    { name: String
    , description: Maybe String
    }

type alias WelcomeModel =
    { wishlistName: String
    , wishlistDescription: String
    }

type alias EditWishModalModel =
    { wish: Wish
    , visibility: Modal.Visibility
    , isInSavingState: Bool
    }

type alias WishlistLoadedModel =
    { wishlist: Wishlist
    , currentToken: Maybe String
    , newWishName: String
    , newWishDescription: String
    , newWishUrl: String
    , editWishModal: Maybe EditWishModalModel
    }
    
type alias LoadingWishlistModel =
    { wishlistId: UUID
    , currentToken: Maybe String
    }
    
type alias Model =
    { state: PageState
    , zone: Time.Zone
    , now: Time.Posix
    , navKey: Key
    , baseUrl: String
    }

type alias Flags =
    { }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        token = tryFromQuery "token" url
        route = (Url.Parser.parse routeParser url) |> Maybe.withDefault NotFoundRoute
        
        (state, command) =
            case route of
                WishlistRoute id -> (LoadingWishlistState { currentToken = token, wishlistId = id  }, Just (requestWishlistFromApi apiUrl id))
                
                WelcomeRoute -> (WelcomeState { wishlistName = "", wishlistDescription = "" }, Nothing)
                
                NotFoundRoute -> (NotFoundState, Nothing)

        getTimeZoneCommand =
            Task.perform GotTimezone Time.here

        commands =
            case command of
                Just c -> [ getTimeZoneCommand, c ]
                Nothing -> [ getTimeZoneCommand ]

        baseUrl =
            (case url.protocol of
                Url.Http  -> "http://"
                Url.Https -> "https://"
            ) ++ url.host ++ 
            (case url.port_ of
                Just i -> ":" ++ (i |> String.fromInt)
                Nothing -> ""
            ) ++ "/"
    in
    ( { state = state, zone = Time.utc, now = Time.millisToPosix 0, navKey = key, baseUrl = baseUrl } , Cmd.batch commands )


-- UPDATE

type Msg
  = UrlChanged Url
  | LinkClicked UrlRequest
  | MessageForWelcome WelcomeMsg
  | MessageForWishlistLoaded WishlistLoadedMsg
  | MessageForLoadingWishlist LoadingWishlistMsg
  | GotTimezone Time.Zone
  | GotNow Time.Posix
  | Copy String
  | CopyResult Bool


type WelcomeMsg
  = NameChange String
  | DescriptionChange String
  | GotNewWishlist (Result Http.Error NewWishlist)
  | CreateNewWishlist
  
  
type WishlistLoadedMsg  
  = UpdatedWishlist ((Result Http.Error Wishlist), Bool)
  | DeleteWish UUID UUID String
  | ShowEditWishModal Wish
  | CloseEditWishModal
  | EditWishName String
  | EditWishDescription String
  | EditWishPriority String
  | EditWishUrl String
  | SaveUpdatedWish
  | MarkWishAsCompleted UUID UUID
  | MarkWishAsNotCompleted UUID UUID
  | NewWishNameChanged String
  | NewWishDescriptionChanged String
  | NewWishUrlChanged String
  | AddWish UUID String NewWish
  

type LoadingWishlistMsg
  = GotWishlistFromApi (Result Http.Error Wishlist)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChanged _ ->
        ( model, Cmd.none )

    LinkClicked _ ->
        ( model, Cmd.none )

    GotTimezone zone ->
        ( { model | zone = zone }, Cmd.none )
        
    GotNow time ->
        ( { model | now = time }, Cmd.none )

    MessageForWelcome welcomeMessage ->
        case model.state of
            WelcomeState welcomeModel ->
                updateWelcome welcomeMessage welcomeModel model
            _ ->
                let _ = Debug.log "MessageForWelcome" "Received MessageForWelcome not in WelcomeState"
                in 
                ( model, Cmd.none)
                
    MessageForWishlistLoaded wishlistLoadedMessage ->
        case model.state of
            WishlistLoadedState loadedModel ->
                updateWishlistLoaded wishlistLoadedMessage loadedModel model
            _ ->
                let _ = Debug.log "MessageForwishlistLoaded" "Received MessageForwishlistLoaded not in WishlistLoadedState"
                in 
                ( model, Cmd.none)

    MessageForLoadingWishlist loadingWishlistMessage ->
        case model.state of
            LoadingWishlistState loadingModel ->
                updateLoadingWishlist loadingWishlistMessage loadingModel model
            _ ->
                let _ = Debug.log "MessageForLoadingWishlist" "Received MessageForLoadingWishlist not in LoadingWishlistState"
                in 
                ( model, Cmd.none)

    Copy string ->
        ( model, copy string )   
       
    CopyResult _ ->
        ( model, Cmd.none )   
       

updateWelcome : WelcomeMsg -> WelcomeModel -> Model -> (Model, Cmd Msg)
updateWelcome msg welcomeModel model =
    case msg of
        NameChange newName ->
            ( { model | state = WelcomeState { welcomeModel | wishlistName = newName } }, Cmd.none )
            
        DescriptionChange newDescription ->
            ( { model | state = WelcomeState { welcomeModel | wishlistDescription = newDescription } }, Cmd.none )

        GotNewWishlist retrievalResult ->
            case retrievalResult of
                Ok newWishlist ->
                    {- There are two commands that need to be run once a new wish list was received:
                        * get the current time from the browser
                        * change the url to include the id of the wish list
                    -}
                    let
                        cmd =
                            [ Nav.pushUrl model.navKey ("/wishlists/" ++ (newWishlist.wishlist.id |> UUID.toString) ++ "?token=" ++ newWishlist.token)
                            , Task.perform GotNow Time.now
                            ]
                    in
                    ( { model | state =
                        WishlistLoadedState 
                        { wishlist = newWishlist.wishlist
                        , currentToken = Just newWishlist.token
                        , newWishName = ""
                        , newWishDescription = ""
                        , newWishUrl = ""
                        , editWishModal = Nothing
                        } }, Cmd.batch cmd )
                Err e ->
                    let
                       _ = Debug.log "Retrieving the result of a create-new-wishlist-request is in error state" e
                    in
                    ( { model | state = ErrorState ("Wunschliste konnte nicht angelegt werden", e |> httpErrorToString) }, Cmd.none )

        CreateNewWishlist ->
            let
                description = 
                    if welcomeModel.wishlistDescription |> String.isEmpty then Nothing
                    else Just welcomeModel.wishlistDescription
                newWishlistRequest = { name = welcomeModel.wishlistName, description = description }
            in
            ( model, registerNewWishlistFromApi (apiUrl ++ "/wishlists") newWishlistRequest)


updateWishlistLoaded : WishlistLoadedMsg -> WishlistLoadedModel -> Model -> (Model, Cmd Msg)
updateWishlistLoaded msg loadedModel model =
    case msg of
        UpdatedWishlist retrievalResult ->
            case retrievalResult of
                (Ok wishlist, clearNewWishForm) ->
                    let
                        updatedLoadedModel =
                            if clearNewWishForm then
                                { loadedModel |
                                    wishlist = wishlist,
                                    newWishName = "",
                                    newWishUrl = "",
                                    newWishDescription = "",
                                    editWishModal = Nothing
                                }
                            else
                                { loadedModel | wishlist = wishlist }
                    in
                    ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )
                (Err e, _) ->
                    let
                       _ = Debug.log "The server refused the request" e
                    in
                    ( { model | state = ErrorState ("Der Server hat die Anfrage abgelehnt =(", e |> httpErrorToString) }, Cmd.none )

        DeleteWish wishlistId wishId authToken ->
            ( model, deleteWishFromApi wishlistId wishId authToken )

        ShowEditWishModal wish ->
            let
                editWish =
                    { id = wish.id
                    , name = wish.name
                    , description = wish.description
                    , priority = wish.priority
                    , urls = wish.urls
                    , isCompleted = wish.isCompleted
                    , creationTime = wish.creationTime
                    }
                editModalModel =
                    { wish = editWish
                    , visibility = Modal.shown
                    , isInSavingState = False
                    }
                updatedLoadedModel = { loadedModel | editWishModal = Just editModalModel }
            in
            ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )
            
        SaveUpdatedWish ->
            let
                (newModel, cmd) =
                    case loadedModel.editWishModal of
                        Just modal ->
                            (model, updateWishInApi loadedModel.wishlist.id (loadedModel.currentToken |> Maybe.withDefault "") modal.wish)
                        Nothing ->
                            ({ model | state = ErrorState ("Wunsch konnte nicht aktualisiert werden", "Der Anwendungsstatus ist korrupt =(") }, Cmd.none)
            in
            ( newModel, cmd)
            
        CloseEditWishModal ->
            let
                updatedLoadedModel = { loadedModel | editWishModal = Nothing }
            in
            ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )
            
        EditWishName newName ->
            let
                state =
                    case loadedModel.editWishModal of
                        Just modal ->
                            let
                                wish = modal.wish
                                updatedWish = { wish | name = newName }
                                updatedEditModal = { modal | wish = updatedWish }
                            in
                            WishlistLoadedState { loadedModel | editWishModal = Just updatedEditModal }
                        Nothing ->
                            ErrorState ("Wunsch konnte nicht aktualisiert werden", "Der Anwendungsstatus ist korrupt =(")
            in
            ( { model | state = state }, Cmd.none )

        EditWishDescription newDescription ->
            let
                state =
                    case loadedModel.editWishModal of
                        Just modal ->
                            let
                                wish = modal.wish
                                updatedWish = { wish | description = if newDescription |> String.isEmpty then Nothing else Just newDescription }
                                updatedEditModal = { modal | wish = updatedWish }
                            in
                            WishlistLoadedState { loadedModel | editWishModal = Just updatedEditModal }
                        Nothing ->
                            ErrorState ("Wunsch konnte nicht aktualisiert werden", "Der Anwendungsstatus ist korrupt =(")
            in
            ( { model | state = state }, Cmd.none )

        EditWishUrl newUrl ->
            let
                state =
                    case loadedModel.editWishModal of
                        Just modal ->
                            let
                                wish = modal.wish
                                updatedWish = { wish | urls = if newUrl |> String.isEmpty then [] else [ newUrl ] }
                                updatedEditModal = { modal | wish = updatedWish }
                            in
                            WishlistLoadedState { loadedModel | editWishModal = Just updatedEditModal }
                        Nothing ->
                            ErrorState ("Wunsch konnte nicht aktualisiert werden", "Der Anwendungsstatus ist korrupt =(")
            in
            ( { model | state = state }, Cmd.none )
            
        EditWishPriority rawPriority ->
            let
                newPriority =
                    case rawPriority |> String.toLower of
                        "low" -> Just Low
                        "moderate" -> Just Moderate
                        "high" -> Just High
                        "veryhigh" -> Just VeryHigh
                        _ -> Nothing
                state =
                    case loadedModel.editWishModal of
                        Just modal ->
                            let
                                wish = modal.wish
                                updatedWish = { wish | priority = newPriority }
                                updatedEditModal = { modal | wish = updatedWish }
                            in
                            WishlistLoadedState { loadedModel | editWishModal = Just updatedEditModal }
                        Nothing ->
                            ErrorState ("Wunsch konnte nicht aktualisiert werden", "Der Anwendungsstatus ist korrupt =(")
            in
            ( { model | state = state }, Cmd.none )
            
        MarkWishAsCompleted wishlistId wishId ->
            ( model, markWishAsCompleted wishlistId wishId)

        MarkWishAsNotCompleted wishlistId wishId ->
            ( model, markWishAsNotCompleted wishlistId wishId)

        NewWishNameChanged string ->
            let
                updatedLoadedModel = { loadedModel | newWishName = string }
            in
            ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )

        NewWishDescriptionChanged string ->
            let
                updatedLoadedModel = { loadedModel | newWishDescription = string }
            in
            ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )
            
        NewWishUrlChanged string ->
            let
                updatedLoadedModel = { loadedModel | newWishUrl = string }
            in
            ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )
            
        AddWish wishlistId token newWish ->
            ( model, addWishToApi wishlistId token newWish )
            
            
updateLoadingWishlist : LoadingWishlistMsg -> LoadingWishlistModel -> Model -> (Model, Cmd Msg)
updateLoadingWishlist msg loadingModel model =
    case msg of
        GotWishlistFromApi retrievalResult ->
            case retrievalResult of
                Ok wishlist ->
                    ( { model | state =
                        WishlistLoadedState
                        { wishlist = wishlist
                        , currentToken = loadingModel.currentToken 
                        , newWishUrl = ""
                        , newWishDescription = ""
                        , newWishName = ""
                        , editWishModal = Nothing
                        } }, Task.perform GotNow Time.now )
                Err e ->
                    let
                       _ = Debug.log "Retrieving the result of a create-new-wishlist-request is in error state" e
                    in
                    ( { model | state = ErrorState ("Wunschliste konnte nicht geladen werden", e |> httpErrorToString) }, Cmd.none )


-- VIEW

mainView : Model -> Browser.Document Msg
mainView model =
    let
        title =
            case model.state of
                WelcomeState _ -> "Wishes - Erstelle deine Wunschliste!"
                NotFoundState -> "Wishes - Url nicht gefunden =("
                LoadingWishlistState _ -> "Wishes - Liste wird geladen"
                WishlistLoadedState loadedModel -> "Wishes - " ++ loadedModel.wishlist.name
                ErrorState _ -> "Wishes - Es gab ein Problem =("
        view =
            case model.state of
                WelcomeState m -> viewWelcome m
                NotFoundState -> viewNotFound
                LoadingWishlistState m -> viewLoadingWishlist m
                WishlistLoadedState m -> viewWishlistLoaded model m
                ErrorState e -> viewErrorState e
    in
    { title = title
    , body =
        [ Grid.container []
          [ view
          ]
        ]
    }
    
    
viewWelcome : WelcomeModel -> Html Msg
viewWelcome model =
    let
        nameInput = model.wishlistName |> Input.value
        descriptionInput = model.wishlistDescription |> Input.value
    in
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [
    div [ ]
    [ h1 [] [ text "Erstelle deine eigene Wunschliste =)"]
    , div [] 
      [ Form.form []
        [ Form.group []
          [ Form.label [ for "name" ] [ text "Name" ] 
          , Input.text [ Input.id "name", nameInput, onInput (\s -> MessageForWelcome (NameChange s))]
          , Form.help [] [ text "Name der Wunschliste"]
          ]
        , Form.group []
          [ Form.label [ for "description" ] [ text "Details" ]
          , Input.text [ Input.id "description", descriptionInput, onInput (\s -> MessageForWelcome (DescriptionChange s)) ]
          , Form.help [ ] [ text "Optionale Beschreibung der Wunschliste"]
          ]
        ]
        , Button.button [ Button.primary, Button.onClick (MessageForWelcome CreateNewWishlist) ] [ text "Anlegen" ]
      ]
    ]
    ]
    
    
viewNotFound : Html Msg
viewNotFound =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [ h1 [] [ text "404 - Seite nicht gefunden =("]
    ]


viewLoadingWishlist : LoadingWishlistModel -> Html Msg
viewLoadingWishlist _ =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [ h1 [] [ text "Wunschliste wird geladen, einen Moment bitte..."]
    ]

-- View if a wishlist has been loaded

viewWishlistLoaded : Model -> WishlistLoadedModel -> Html Msg
viewWishlistLoaded model loadedModel =
    let
        subTitle =
            loadedModel.wishlist.description
            |> Maybe.map (\t -> h4 [] [ text t ])
            |> Maybe.withDefault (div [] [])
        formattedAge = createAgeText "Gerade eben angelegt! 😱" "Angelegt vor " "" loadedModel.wishlist.creationTime model.now
        isAuthTokenSet = loadedModel.currentToken |> Maybe.isJust
        token = (loadedModel.currentToken |> Maybe.withDefault "")
        wishes =
            if loadedModel.wishlist.wishes |> List.isEmpty then
                h4 [ Spacing.mt2 ] [ text "Diese Liste hat aktuell keine Einträge"]
            else 
                Html.ul [ class "list-group" ] ( loadedModel.wishlist.wishes |> List.map (viewWish isAuthTokenSet loadedModel.wishlist.id token model.now) )
        isAddButtonDisabled = loadedModel.newWishName |> String.isEmpty
        controls =
            if loadedModel.currentToken |> Maybe.isJust then
                Card.config [ Card.attrs [ Spacing.mt2, Spacing.mb2 ] ] 
                |> Card.header [ class "text-center" ]
                    [ Html.h5 [ Spacing.m0 ] [ text "Wunsch hinzufügen" ]
                    ]
                |> Card.block []
                [
                  Block.custom <|
                      (Form.form []
                        [ Form.group []
                          [ Form.label [ for "newWishName" ] [ text "Wunsch" ]
                          , Input.text [ Input.id "newWishName", Input.value loadedModel.newWishName, onInput (\x -> MessageForWishlistLoaded (NewWishNameChanged x)) ]
                          ]
                        , Form.group []
                          [ Form.label [ for "newWishDescription" ] [ text "Details (optional)" ]
                          , Input.text [ Input.id "newWishDescription", Input.value loadedModel.newWishDescription, onInput (\x -> MessageForWishlistLoaded (NewWishDescriptionChanged x)) ]
                          ]
                        , Form.group []
                          [ Form.label [ for "newWishUrl" ] [ text "Link (optional)" ]
                          , Input.url [ Input.id "newWishUrl", Input.value loadedModel.newWishUrl, onInput (\x -> MessageForWishlistLoaded (NewWishUrlChanged x)) ]
                          ]
                        ])
                , Block.custom <|
                    (Button.button [ Button.primary, Button.attrs [ disabled isAddButtonDisabled, onClick (MessageForWishlistLoaded (AddWish loadedModel.wishlist.id token { name = loadedModel.newWishName, description = loadedModel.newWishDescription, url = loadedModel.newWishUrl } )) ] ] [ text "Hinzufügen" ])
                ]
                |> Card.view
            else
                Card.config []
                |> Card.block [] [ Block.text [] [ text "Wünsche können nicht ohne den Admin-Schlüssel hinzugefügt/gelöscht werden" ] ]
                |> Card.view
        
        copyButtons =
            let
                buttons =
                    let
                        userUrl = model.baseUrl ++ "wishlists/" ++ (loadedModel.wishlist.id |> UUID.toString)
                        adminUrl = userUrl ++ "?token=" ++ (loadedModel.currentToken |> Maybe.withDefault "")
                    in
                    if isAuthTokenSet then
                        [ Button.button [ Button.primary, Button.attrs [ onClick (Copy userUrl) ] ] [ text "Kopiere Teilen-Link" ]
                        , Button.button [ Button.primary, Button.attrs [ onClick (Copy adminUrl), Spacing.ml2 ] ] [ text "Kopiere Admin-Link" ]
                        ]
                    else
                        [ Button.button [ Button.primary, Button.attrs [ onClick (Copy userUrl) ] ] [ text "Kopiere Teilen-Link" ] ]
            in
            div [ Spacing.mt2, Spacing.mb2 ] buttons
            
        shortCut =
            let
                buttons =
                    if isAuthTokenSet then
                        [ Button.button [ Button.outlineDark, Button.attrs [ onClick (Copy (loadedModel.wishlist.id |> UUID.toString)) ] ] [ text "Kopiere Id"]
                        , Button.button [ Button.outlineDark, Button.attrs [ onClick (Copy (loadedModel.currentToken |> Maybe.withDefault "")), Spacing.ml2 ] ] [ text "Kopire Token"]
                        , Button.linkButton [ Button.outlineDark, Button.attrs [ href "https://www.icloud.com/shortcuts/090566f0339f4b61a3add7fd7e2c2c7e", Spacing.ml2, Spacing.p0 ] ] [ Html.img [ Html.Attributes.width 35, Html.Attributes.src "/shortcut.png" ] [] ]
                        ]
                    else []
            in
            div [ ] buttons
            
        asFullRowCol html =
            Grid.row [] [ Grid.col [] [ html ] ]
            
        modal =
            case loadedModel.editWishModal of
                Just editWishModal ->
                    div [] [ viewEditWishModal Modal.shown editWishModal ]
                Nothing ->
                    div [] []
    in
    div []
    [ h1 [ Spacing.mt2 ] [ text loadedModel.wishlist.name ] |> asFullRowCol
    , modal
    , subTitle |> asFullRowCol
    , small [ class "text-muted" ] [ text formattedAge ] |> asFullRowCol
    , copyButtons |> asFullRowCol
    -- , shortCut |> asFullRowCol
    , controls |> asFullRowCol
    , wishes |> asFullRowCol
    ]


viewWish : Bool -> UUID -> String -> Time.Posix -> Wish -> Html Msg
viewWish showDeleteButton wishlistId authToken now wish =
    let
        priority =
            case wish.priority of
                Just p ->
                    case p of
                        Low -> "(nicht so wichtig)"
                        Moderate -> "(wäre nett)"
                        High -> "(wichtig)"
                        VeryHigh -> "(sehr wichtig)"
                Nothing -> ""
        title =
            if wish.isCompleted then
                Block.custom (div [ Flex.block, Flex.alignItemsBaseline ] [ Html.h5 [] [ Html.del [] [ text wish.name ] ], Html.small [ Spacing.ml2 ] [ text priority ] ])
            else
                Block.custom (div [ Flex.block, Flex.alignItemsBaseline ] [ Html.h5 [] [ text wish.name ], Html.small [ Spacing.ml2 ] [ text priority ] ])

        description =
            wish.description
            |> Maybe.map (\t -> Block.text [] [ text t ])
                
        links =
            let
                maxLength = 35
                createName fullLink =
                    if (fullLink |> String.length) > maxLength then
                        (fullLink |> String.left maxLength) ++ "..."
                    else fullLink
            in
            wish.urls |> List.map (\u -> Just (Block.link [ href u ] [ text (createName u)]))

        blocks =
            [ Just title
            , description
            ]
            
        completionButton =
            let
                (buttonText, buttonMsg) =
                    if wish.isCompleted then
                        ("Als nicht gekauft markieren", MessageForWishlistLoaded (MarkWishAsNotCompleted wishlistId wish.id))
                    else
                        ("Als gekauft markieren", MessageForWishlistLoaded (MarkWishAsCompleted wishlistId wish.id))
            in
            Just (Button.button [ Button.primary, Button.small, Button.attrs [ Spacing.mr2, onClick buttonMsg ] ] [ text buttonText ])

        deletionButton =
            if showDeleteButton then
                Just (Button.button [ Button.danger, Button.small, Button.attrs [ Html.Attributes.style "width" "35px", onClick (MessageForWishlistLoaded (DeleteWish wishlistId wish.id authToken)) ] ] [ text "☓"])
            else Nothing
            
        editButton =
            if showDeleteButton then
                Just (Button.button [ Button.primary, Button.small, Button.attrs [ Html.Attributes.style "width" "35px", Spacing.mr2, onClick (MessageForWishlistLoaded (ShowEditWishModal wish)) ] ] [ text "🖉"])
            else Nothing
            
        buttons =
            Just (Block.custom <| div [ Spacing.mt2 ] ([ completionButton, editButton, deletionButton ] |> Maybe.values))
            
        all = (List.append (List.append blocks links) [ buttons ]) |> Maybe.values
        
        age = createAgeText "Gerade eben hinzugefügt! 😱" "Hinzugefügt vor " "" wish.creationTime now
        
        mutedClass = if wish.isCompleted then "text-muted" else ""
        
        background = if wish.isCompleted then style "background" "#e4e4e4" else style "" ""
    in
    Card.config [ Card.attrs [ class mutedClass, Spacing.mb2, background ] ]
        |> Card.block [] all
        |> Card.footer [] [ small [] [ text age ] ]
        |> Card.view


viewEditWishModal : Modal.Visibility -> EditWishModalModel -> Html Msg
viewEditWishModal visibility editModel =
    let
        createSelect selectPriority selectValue selectName =
            Select.item [ Html.Attributes.value selectValue, Html.Attributes.selected (selectPriority == editModel.wish.priority) ] [ text selectName ] 
            
        name = Input.text [ Input.placeholder "Name", Input.id "editWishName", editModel.wish.name |> Input.value, Input.attrs [ Spacing.mb2 ] , onInput (\s -> MessageForWishlistLoaded (EditWishName s)) ]
        description = Input.text [ Input.placeholder "Details", Input.id "editWishDescription", (editModel.wish.description |> Maybe.withDefault "") |> Input.value, Input.attrs [ Spacing.mb2 ], onInput (\s -> MessageForWishlistLoaded (EditWishDescription s)) ]
        priority = Select.select [ Select.id "editWishPriority", Select.onChange (\p -> MessageForWishlistLoaded (EditWishPriority p)) ]
                    [ createSelect Nothing "none" "(keine)"
                    , createSelect (Just Low) "low" "nicht so wichtig"
                    , createSelect (Just Moderate) "moderate" "wäre nett"
                    , createSelect (Just High) "high" "wichtig"
                    , createSelect (Just VeryHigh) "veryhigh" "sehr wichtig"
                    ]
        
        url =
            let
                val =
                    editModel.wish.urls |> List.head |> Maybe.withDefault ""
            in
            Input.url 
                [ Input.placeholder "Link"
                , Input.id "editWishUrl"
                , val |> Input.value
                , onInput (\u -> MessageForWishlistLoaded (EditWishUrl u))
                , Input.attrs 
                    [ Spacing.mt2
                    ]
                ]
    
        body =
            [ name, description, priority, url ]
    in
    Modal.config (MessageForWishlistLoaded CloseEditWishModal)
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Wunsch bearbeiten" ]
        |> Modal.body [] body --[ name, description, priority, separator ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlineDanger
                , Button.attrs [ onClick (MessageForWishlistLoaded CloseEditWishModal) ]
                ]
                [ text "Abbrechen"
                ]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick (MessageForWishlistLoaded SaveUpdatedWish) ]
                ]
                [ text "Speichern"
                ]
            ]
        |> Modal.view visibility


viewErrorState : (String, String) -> Html Msg
viewErrorState (error, details) =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [
      div []
      [ h1 [] [ text "Entschuldigung, es kam zu einem Fehler =("]
      , h4 [] [ text error ]
      , small [] [ text details ]
      ]
    ]


createAgeText : String -> String -> String -> Time.Posix -> Time.Posix -> String
createAgeText justNowText prefix suffix start end =
    let
        age =
            Duration.from start end
        ageInSeconds = age |> Duration.inSeconds
        ageInMinutes = age |> Duration.inMinutes
        ageInHours = age |> Duration.inHours
        ageInDays = age |> Duration.inDays
        ageInMonths = ageInDays / 30.437
        ageInYears = age |> Duration.inJulianYears
        formattedAge =
            if ageInSeconds <= 10 then justNowText
            else if ageInMinutes <= 1 then prefix ++ (ageInSeconds |> Round.floor 0) ++ " Sekunden" ++ suffix
            else if ageInHours <= 1 then prefix ++ (ageInMinutes |> Round.floor 0) ++ " Minuten" ++ suffix
            else if ageInDays <= 1 then prefix ++ (ageInHours |> Round.floor 0) ++ " Stunden" ++ suffix
            else if ageInMonths <= 1 then prefix ++ (ageInDays |> Round.floor 0) ++ " Tagen" ++ suffix
            else if ageInYears <= 1 then prefix ++ (ageInMonths |> Round.floor 0) ++ " Monaten" ++ suffix
            else prefix ++ (ageInYears |> Round.floor 0) ++ " Jahren" ++ suffix
    in formattedAge
