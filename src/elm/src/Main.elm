module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Form.Input as Input exposing (onInput)
import Bootstrap.Text as Text

import Browser
import Dict
import Html exposing (Html, button, div, h1, h4, small, text)
import Html.Attributes exposing (class, for, href, style)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Encode
import Maybe.Extra as Maybe
import Route exposing (Route)
import Task
import Time
import UUID exposing (UUID)
import Url exposing (Protocol(..), Url)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import QS as QS
import Url.Parser exposing (Parser, (</>), map, oneOf, s)
import Json.Decode exposing (Decoder, at, bool, field, list, maybe, string)
import Duration as Duration
import Round exposing (floor)


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


requestWishlistFromApi : String -> Json.Encode.Value -> Cmd Msg
requestWishlistFromApi url payload =
    Http.post
      { url = url
      , body = Http.jsonBody payload
      , expect = Http.expectJson (\r -> (MessageForWelcome (GotNewWishlist r))) newWishlistDecoder
      }
      
      
{-| Sends the minimum amount of fields required to create a new wishlist on the backend side -}
registerNewWishlistFromApi : String ->  NewWishlistRequest -> Cmd Msg
registerNewWishlistFromApi url wishlist =
    requestWishlistFromApi url (wishlist |> newWishlistEncoder)
      
      
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
        

wishesDecoder : Decoder (List Wish)
wishesDecoder =
    list wishDecoder
    

wishDecoder : Decoder Wish
wishDecoder =
    Json.Decode.map6 Wish
        (field "id" UUID.jsonDecoder)
        (field "name" string)
        (field "description" (maybe string))
        (field "urls" (list string))
        (field "isCompleted" bool)
        (field "creationTime" Iso8601.decoder)


-- MODEL

type PageState
    = NotFoundState
    | LoadingWishlistState LoadWishlistModel
    | WishlistLoadedState WishlistLoadedModel
    | WelcomeState WelcomeModel
    
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
    }
    
type alias LoadWishlistModel =
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
init flags url key =
    let
        token = tryFromQuery "token" url
        route =  (Url.Parser.parse routeParser url) |> Maybe.withDefault NotFoundRoute
        
        state =
            case route of
                WishlistRoute id -> LoadingWishlistState { currentToken = token, wishlistId = id  }
                
                WelcomeRoute -> WelcomeState { wishlistName = "", wishlistDescription = "" }
                
                NotFoundRoute -> NotFoundState

        _ = Debug.log "state" state
    in
    ( { state = state, zone = Time.utc, now = Time.millisToPosix 0, navKey = key } , Task.perform GotTimezone Time.here )


-- UPDATE

type Msg
  = UrlChanged Url
  | LinkClicked UrlRequest
  | MessageForWelcome WelcomeMsg
  | MessageForWishlistLoaded WishlistLoadedMsg
  | GotTimezone Time.Zone
  | GotNow Time.Posix


type WelcomeMsg
  = NameChange String
  | DescriptionChange String
  | GotNewWishlist (Result Http.Error NewWishlist)
  | CreateNewWishlist
  
  
type WishlistLoadedMsg  
  = GetTimeAndZone Time.Posix
  

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
                            [ Task.perform GotNow Time.now
                            , Nav.pushUrl model.navKey (newWishlist.wishlist.id |> UUID.toString)
                            ]
                    in
                    ( { model | state = WishlistLoadedState { wishlist = newWishlist.wishlist, currentToken = Just newWishlist.token } }, Cmd.batch cmd )
                Err e ->
                    let
                        _ = Debug.log "Retrieving the result of a create-new-wishlist-request is in error state" e
                    in
                    ( model, Cmd.none )

        CreateNewWishlist ->
            let
                description = 
                    if welcomeModel.wishlistDescription |> String.isEmpty then Nothing
                    else Just welcomeModel.wishlistDescription
                newWishlistRequest = { name = welcomeModel.wishlistName, description = description }
            in
            ( model, registerNewWishlistFromApi (apiUrl ++ "/wishlists") newWishlistRequest)


updateWishlistLoaded : WishlistLoadedMsg -> WishlistLoadedModel -> Model -> (Model, Cmd Msg)
updateWishlistLoaded msg loadedMsg model =
    ( model, Cmd.none )


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
        view =
            case model.state of
                WelcomeState m -> viewWelcome m
                NotFoundState -> viewNotFound
                LoadingWishlistState m -> viewLoadingWishlist m
                WishlistLoadedState m -> viewWishlistLoaded model m
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

viewLoadingWishlist : LoadWishlistModel -> Html Msg
viewLoadingWishlist model =
    div [ Flex.block, Flex.justifyCenter, Flex.alignItemsCenter, style "height" "calc(100vh)" ]
    [ h1 [] [ text "We are loading your wishlist, please wait..."]
    ]

-- View if a wishlist has been loaded

viewWish : Wish -> Html Msg
viewWish wish =
   Html.a [ href "#", class "list-group-item list-group-item-action flex-column align-items-start" ]
   [
   ] 

viewWishlistLoaded : Model -> WishlistLoadedModel -> Html Msg
viewWishlistLoaded model loadedModel =
    let
        subTitle =
            loadedModel.wishlist.description
            |> Maybe.map (\t -> h4 [] [ text t ])
            |> Maybe.withDefault (div [] [])
        age =
            Duration.from model.now loadedModel.wishlist.creationTime
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
        wishes =
            if loadedModel.wishlist.wishes |> List.isEmpty then [ h4 [] [ text "There are no wishes on this list"] ]
            else loadedModel.wishlist.wishes |> List.map viewWish
        controls =
            if loadedModel.currentToken |> Maybe.isJust then
                div [] []
            else
                div [] [ text "You cannot add or delete wishes from this wish list unless you have the matching admin token"]
    in
    div []
    [ h1 [] [ text loadedModel.wishlist.name ]
    , subTitle
    , small [ class "text-muted" ] [ text formattedAge ]
    , controls
    , div [] wishes
    ]