module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
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


apiUrl : String
apiUrl = "http://localhost:5000"


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
    Sub.none


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
        , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist r))) wishlistDecoder
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
    , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist r))) addWishToWishlistDecoder
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
      , expect = Http.expectJson (\r -> (MessageForWishlistLoaded (UpdatedWishlist r))) wishlistDecoder
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
    

wishDecoder : Decoder Wish
wishDecoder =
    Json.Decode.map6 Wish
        (field "id" UUID.jsonDecoder)
        (field "name" string)
        (maybe (field "description" string))
        (field "urls" (list string))
        (field "isCompleted" bool)
        (field "creationTime" Iso8601.decoder)


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

type alias Wish =
    { id: UUID
    , name: String
    , description: Maybe String
    , urls: List String
    , isCompleted: Bool
    , creationTime: Time.Posix
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

type alias WishlistLoadedModel =
    { wishlist: Wishlist
    , currentToken: Maybe String
    , newWishName: String
    , newWishDescription: String
    , newWishUrl: String
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
                    
        _ = Debug.log "state" state
    in
    ( { state = state, zone = Time.utc, now = Time.millisToPosix 0, navKey = key } , Cmd.batch commands )


-- UPDATE

type Msg
  = UrlChanged Url
  | LinkClicked UrlRequest
  | MessageForWelcome WelcomeMsg
  | MessageForWishlistLoaded WishlistLoadedMsg
  | MessageForLoadingWishlist LoadingWishlistMsg
  | GotTimezone Time.Zone
  | GotNow Time.Posix


type WelcomeMsg
  = NameChange String
  | DescriptionChange String
  | GotNewWishlist (Result Http.Error NewWishlist)
  | CreateNewWishlist
  
  
type WishlistLoadedMsg  
  = UpdatedWishlist (Result Http.Error Wishlist)
  | DeleteWish UUID UUID String
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
        let _ = Debug.log "GotTimezone" zone
        in
        ( { model | zone = zone }, Cmd.none )
        
    GotNow time ->
        let _ = Debug.log "GotTime" time
        in
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


updateWelcome : WelcomeMsg -> WelcomeModel -> Model -> (Model, Cmd Msg)
updateWelcome msg welcomeModel model =
    case msg of
        NameChange newName ->
            ( { model | state = WelcomeState { welcomeModel | wishlistName = newName } }, Cmd.none )
            
        DescriptionChange newDescription ->
            ( { model | state = WelcomeState { welcomeModel | wishlistDescription = newDescription } }, Cmd.none )

        GotNewWishlist retrievalResult ->
            let
                _ = Debug.log "GotNewWishlist" retrievalResult
            in
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
                        } }, Cmd.batch cmd )
                Err e ->
                    let
                        _ = Debug.log "Retrieving the result of a create-new-wishlist-request is in error state" e
                    in
                    ( { model | state = ErrorState ("Could not register the new wish list", e |> httpErrorToString) }, Cmd.none )

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
            let
                _ = Debug.log "UpdateWishlist" retrievalResult
            in
            case retrievalResult of
                Ok wishlist ->
                    let
                        updatedLoadedModel =
                            { loadedModel | wishlist = wishlist}
                    in
                    ( { model | state = WishlistLoadedState updatedLoadedModel }, Cmd.none )
                Err e ->
                    let
                        _ = Debug.log "The server refused the request =(" e
                    in
                    ( { model | state = ErrorState ("The server refused the request =(", e |> httpErrorToString) }, Cmd.none )

        DeleteWish wishlistId wishId authToken ->
            ( model, deleteWishFromApi wishlistId wishId authToken )

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
    let
        _ = Debug.log "updateLoadingWishlist" msg
    in
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
                        } }, Task.perform GotNow Time.now )
                Err e ->
                    let
                        _ = Debug.log "Retrieving the result of a create-new-wishlist-request is in error state" e
                    in
                    ( { model | state = ErrorState ("Could not load the wish list", e |> httpErrorToString) }, Cmd.none )


-- VIEW

mainView : Model -> Browser.Document Msg
mainView model =
    let
        title =
            case model.state of
                WelcomeState _ -> "Wishes - Create a new, free wishlist here!"
                NotFoundState -> "Wishes - Url not found :("
                LoadingWishlistState _ -> "Wishes - Please wait while we load your wishlist"
                WishlistLoadedState loadedModel -> "Wishes - " ++ loadedModel.wishlist.name
                ErrorState _ -> "Wishes - We ran into an error =("
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
    [ h1 [] [ text "Create your free online wish list here"]
    , div [] 
      [ Form.form []
        [ Form.group []
          [ Form.label [ for "name" ] [ text "Name" ] 
          , Input.text [ Input.id "name", nameInput, onInput (\s -> MessageForWelcome (NameChange s))]
          , Form.help [] [ text "The name will be displayed prominently on the wish list page"]
          ]
        , Form.group []
          [ Form.label [ for "description" ] [ text "Description" ]
          , Input.text [ Input.id "description", descriptionInput, onInput (\s -> MessageForWelcome (DescriptionChange s)) ]
          , Form.help [ ] [ text "Add an optional description to the wish list to add some context"]
          ]
        ]
        , Button.button [ Button.primary, Button.onClick (MessageForWelcome CreateNewWishlist) ] [ text "Create" ]
      ]
    ]
    ]
    
    
viewNotFound : Html Msg
viewNotFound =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [ h1 [] [ text "404 - Page not found =("]
    ]

viewLoadingWishlist : LoadingWishlistModel -> Html Msg
viewLoadingWishlist _ =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [ h1 [] [ text "We are loading your wishlist, please wait..."]
    ]

-- View if a wishlist has been loaded

viewWish : Bool -> UUID -> String -> Wish -> Html Msg
viewWish showDeleteButton wishlistId authToken wish =
    let
        name =
            if wish.isCompleted then Html.del [] [ text wish.name ]
            else div [] [ text wish.name ]
            
        description =
            case wish.description of
                Just desc -> Html.p [] [ text desc ]
                Nothing -> div [] []

        urls =
            case wish.urls of
                [] -> 
                    div [] [ text "There are no links" ]
                links ->
                    div [] (links |> List.map (\l -> Html.a [ href l ] [ text l ]))
                    
        completionButton =
            let
                (buttonText, buttonMsg) =
                    if wish.isCompleted then
                        ("Mark as not bought", MessageForWishlistLoaded (MarkWishAsNotCompleted wishlistId wish.id))
                    else
                        ("Mark as bought", MessageForWishlistLoaded (MarkWishAsCompleted wishlistId wish.id))
            in
            Button.button [ Button.primary, Button.small, Button.attrs [ Spacing.mr2, onClick buttonMsg ] ] [ text buttonText ] 

        deletionButton =
            if showDeleteButton then
                Button.button [ Button.danger, Button.small, Button.attrs [ onClick (MessageForWishlistLoaded (DeleteWish wishlistId wish.id authToken)) ] ] [ text "Delete"]
            else div [] []
            
        buttons =
            div [] [ completionButton, deletionButton ]
            
    in
    Html.li [ href "#", Spacing.mt2, class "list-group-item list-group-item-action flex-column align-items-start" ]
    [ h4 [] [ name ]
    , description
    , urls
    , buttons
    ] 

viewWishlistLoaded : Model -> WishlistLoadedModel -> Html Msg
viewWishlistLoaded model loadedModel =
    let
        subTitle =
            loadedModel.wishlist.description
            |> Maybe.map (\t -> h4 [] [ text t ])
            |> Maybe.withDefault (div [] [])
        age =
            Duration.from loadedModel.wishlist.creationTime model.now 
        ageInSeconds = age |> Duration.inSeconds
        ageInMinutes = age |> Duration.inMinutes
        ageInHours = age |> Duration.inHours
        ageInDays = age |> Duration.inDays
        ageInMonths = ageInDays / 30.437
        ageInYears = age |> Duration.inJulianYears
        formattedAge =
            if ageInSeconds <= 10 then "Created just now! 😱"
            else if ageInMinutes <= 1 then "Created " ++ (ageInSeconds |> Round.floor 0) ++ " seconds ago"
            else if ageInHours <= 1 then "Created " ++ (ageInMinutes |> Round.floor 0) ++ " minutes ago"
            else if ageInDays <= 1 then "Created " ++ (ageInHours |> Round.floor 0) ++ " hours ago"
            else if ageInMonths <= 1 then "Created " ++ (ageInDays |> Round.floor 0) ++ " days ago"
            else if ageInYears <= 1 then "Created " ++ (ageInMonths |> Round.floor 0) ++ " months ago"
            else "Created " ++ (ageInYears |> Round.floor 0) ++ " years ago"
        isAuthTokenSet = loadedModel.currentToken |> Maybe.isJust
        token = (loadedModel.currentToken |> Maybe.withDefault "")
        wishes =
            if loadedModel.wishlist.wishes |> List.isEmpty then
                h4 [ Spacing.mt2 ] [ text "There are no wishes on this list"]
            else 
                Html.ul [ class "list-group" ] ( loadedModel.wishlist.wishes |> List.map (viewWish isAuthTokenSet loadedModel.wishlist.id token) )
        isAddButtonDisabled = loadedModel.newWishName |> String.isEmpty
        controls =
            if loadedModel.currentToken |> Maybe.isJust then
                Card.config [ Card.attrs [ Spacing.mt2 ] ] 
                |> Card.header [ class "text-center" ]
                    [ Html.h5 [ Spacing.mt2 ] [ text "Add new wish" ]
                    ]
                |> Card.block []
                [
                  Block.custom <|
                      (Form.form []
                        [ Form.group []
                          [ Form.label [ for "newWishName" ] [ text "Wish" ]
                          , Input.text [ Input.id "newWishName", onInput (\x -> MessageForWishlistLoaded (NewWishNameChanged x)) ]
                          ]
                        , Form.group []
                          [ Form.label [ for "newWishDescription" ] [ text "Description (optional)" ]
                          , Input.text [ Input.id "newWishDescription", onInput (\x -> MessageForWishlistLoaded (NewWishDescriptionChanged x)) ]
                          ]
                        , Form.group []
                          [ Form.label [ for "newWishUrl" ] [ text "Link (optional)" ]
                          , Input.url [ Input.id "newWishUrl", onInput (\x -> MessageForWishlistLoaded (NewWishUrlChanged x)) ]
                          ]
                        ])
                , Block.custom <|
                    (Button.button [ Button.primary, Button.attrs [ disabled isAddButtonDisabled, onClick (MessageForWishlistLoaded (AddWish loadedModel.wishlist.id token { name = loadedModel.newWishName, description = loadedModel.newWishDescription, url = loadedModel.newWishUrl } )) ] ] [ text "Add" ])
                ]
                |> Card.view
            else
                Card.config []
                |> Card.block [] [ Block.text [] [ text "You cannot add or delete wishes from this wish list unless you have the matching admin token" ] ]
                |> Card.view
                
        _ = Debug.log "now" model.now
        _ = Debug.log "creationDate" loadedModel.wishlist.creationTime
        _ = Debug.log "age" age
    in
    div []
    [ h1 [] [ text loadedModel.wishlist.name ]
    , subTitle
    , small [ class "text-muted" ] [ text formattedAge ]
    , controls
    , wishes
    ]
    

viewErrorState : (String, String) -> Html Msg
viewErrorState (error, details) =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [
      div []
      [ h1 [] [ text "Sorry, we encountered an error"]
      , h4 [] [ text error ]
      , small [] [ text details ]
      ]
    ]
