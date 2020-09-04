module Home exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



-- MODEL


type alias Model =
    { doc : DocsModel
    , page : Int
    , limit : Int
    , search : String
    , searchMode : Bool
    , form : SongModel
    , editMode : Bool
    }


type alias DocsModel =
    { songs : List SongModel
    , numDocs : Int
    }


type alias SongModel =
    { id : String
    , title : String
    , artist : String
    , album : String
    , image : String
    , preview : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (DocsModel [] 0) 0 20 "" False (SongModel "" "" "" "" "" "") False, getSong 0 20 )



-- UPDATE


type Msg
    = GetSongs
    | DeleteSong String
    | EditSong SongModel
    | CancelEditSong
    | ChangeTitle String
    | ChangeArtist String
    | ChangeAlbum String
    | ChangeImage String
    | ChangePreview String
    | UpdateSong SongModel
    | ClearCreateSong
    | CreateSong SongModel
    | ChangeSearch String
    | SearchSong
    | SearchedSong (Result Http.Error DocsModel)
    | NextPage
    | PrevPage
    | ResetPage (Result Http.Error DocsModel)
    | GotSongs (Result Http.Error DocsModel)
    | DeletedSong (Result Http.Error ())
    | UpdatedSong (Result Http.Error ())
    | CreatedSong (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSongs (Ok result) ->
            ( { model | doc = result }, Cmd.none )

        GotSongs (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )

        GetSongs ->
            ( { model | page = 0 }, resetGetSong 0 20 )

        DeletedSong (Ok result) ->
            ( { model | page = 0 }, resetGetSong 0 20 )

        DeletedSong (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )

        DeleteSong id ->
            ( model, deleteSong id )

        EditSong song ->
            ( { model | form = song, editMode = True }, Cmd.none )

        CancelEditSong ->
            ( { model | form = SongModel "" "" "" "" "" "", editMode = False }, Cmd.none )

        ChangeTitle title ->
            ( { model | form = updateTitle title model.form }, Cmd.none )

        ChangeArtist artist ->
            ( { model | form = updateArtist artist model.form }, Cmd.none )

        ChangeAlbum ablum ->
            ( { model | form = updateAlbum ablum model.form }, Cmd.none )

        ChangeImage image ->
            ( { model | form = updateImage image model.form }, Cmd.none )

        ChangePreview preview ->
            ( { model | form = updatePreview preview model.form }, Cmd.none )

        UpdateSong song ->
            ( { model | form = SongModel song.id song.title song.artist song.album song.image song.preview }, updateSong song )

        UpdatedSong (Ok result) ->
            ( { model | form = SongModel "" "" "" "" "" "", editMode = False }, resetGetSong 0 20 )

        UpdatedSong (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )

        ClearCreateSong ->
            ( { model | form = SongModel "" "" "" "" "" "" }, Cmd.none )

        CreateSong song ->
            ( { model | form = SongModel "" song.title song.artist song.album song.image song.preview }, createSong song )

        CreatedSong (Ok result) ->
            ( { model | form = SongModel "" "" "" "" "" "" }, resetGetSong 0 20 )

        CreatedSong (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )

        ChangeSearch search ->
            ( { model | search = search }, Cmd.none )

        SearchSong ->
            ( { model | searchMode = True, page = 0 }, getSearchSong 0 20 model.search )

        SearchedSong (Ok result) ->
            ( { model | doc = result }, Cmd.none )

        SearchedSong (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )

        NextPage ->
            if model.searchMode then
                ( { model | page = model.page + 1 }, getSearchSong ((model.page + 1) * model.limit) model.limit model.search )

            else
                ( { model | page = model.page + 1 }, getSong ((model.page + 1) * model.limit) model.limit )

        PrevPage ->
            if model.searchMode then
                ( { model | page = model.page - 1 }, getSearchSong ((model.page - 1) * model.limit) model.limit model.search )

            else
                ( { model | page = model.page - 1 }, getSong ((model.page - 1) * model.limit) model.limit )

        ResetPage (Ok result) ->
            ( { model | doc = result, page = 0, searchMode = False, search = "" }, Cmd.none )

        ResetPage (Err e) ->
            ( Debug.log (Debug.toString e) model, Cmd.none )


updateTitle : String -> SongModel -> SongModel
updateTitle val model =
    { model | title = val }


updateArtist : String -> SongModel -> SongModel
updateArtist val model =
    { model | artist = val }


updateAlbum : String -> SongModel -> SongModel
updateAlbum val model =
    { model | album = val }


updateImage : String -> SongModel -> SongModel
updateImage val model =
    { model | image = val }


updatePreview : String -> SongModel -> SongModel
updatePreview val model =
    { model | preview = val }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "position" "fixed"
            , style "margin-top" "0"
            , style "padding-top" "10%"
            , style "padding-left" "10px"
            , style "padding-right" "10px"
            , style "height" "100%"
            , style "width" "300px"
            , style "text-align" "center"
            , style "font-size" "large"
            , style "background-color" "#2e6070"
            , style "color" "#affff1"
            ]
            [ h3 []
                [ if model.editMode then
                    text "Edit Song"

                  else
                    text "Create Song"
                ]
            , hr [] []
            , u [] [ label [ for "title" ] [ text "Title" ] ]
            , br [] []
            , input [ id "title", type_ "text", placeholder "Enter Song Name", style "width" "250px", value model.form.title, onInput ChangeTitle ] []
            , br [] []
            , u [] [ label [ for "artist" ] [ text "Artist" ] ]
            , br [] []
            , input [ id "artist", type_ "text", placeholder "Enter Artist Name", style "width" "250px", value model.form.artist, onInput ChangeArtist ] []
            , br [] []
            , u [] [ label [ for "album" ] [ text "Album" ] ]
            , br [] []
            , input [ id "album", type_ "text", placeholder "Enter Album Name", style "width" "250px", value model.form.album, onInput ChangeAlbum ] []
            , br [] []
            , u [] [ label [ for "image" ] [ text "Album Image Url" ] ]
            , br [] []
            , input [ id "image", type_ "text", placeholder "Enter Image Url", style "width" "250px", value model.form.image, onInput ChangeImage ] []
            , br [] []
            , u [] [ label [ for "audio" ] [ text "Preview Audio Url" ] ]
            , br [] []
            , input [ id "audio", type_ "text", placeholder "Enter Audio Url", style "width" "250px", value model.form.preview, onInput ChangePreview ] []
            , div
                [ if model.editMode then
                    hidden True

                  else
                    hidden False
                ]
                [ button [ onClick ClearCreateSong ] [ text "Clear" ]
                , button [ onClick (CreateSong model.form) ] [ text "Create" ]
                ]
            , div
                [ if model.editMode then
                    hidden False

                  else
                    hidden True
                ]
                [ button [ onClick CancelEditSong ] [ text "Cancel" ]
                , button [ onClick (UpdateSong model.form) ] [ text "Update" ]
                ]
            ]
        , div
            [ style "margin-left" "350px"
            , style "padding" "10px 10px"
            , style "text-align" "center"
            , style "font-size" "large"
            ]
            [ b [ style "font-size" "xx-large" ] [ text "Songs Library " ]
            , div []
                [ button [ onClick GetSongs ] [ text "Reset" ]
                , input [ style "width" "300px", type_ "text", placeholder "Enter song name", value model.search, onInput ChangeSearch ] []
                , button [ onClick SearchSong ] [ text "Search" ]
                ]
            , div []
                [ text
                    ("About "
                        ++ String.fromInt model.doc.numDocs
                        ++ " results, page "
                        ++ String.fromInt (model.page + 1)
                        ++ " of "
                        ++ String.fromInt (numPage model.doc.numDocs model.limit)
                    )
                ]
            , div []
                [ table [ style "margin-left" "auto", style "margin-right" "auto", style "width" "95%", attribute "border" "1px" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Title" ]
                            , th [] [ text "Artist" ]
                            , th [] [ text "Album" ]
                            , th [] [ text "Album Image" ]
                            , th [] [ text "Preview" ]
                            , th [] [ text "Action" ]
                            ]
                        ]
                    , tbody [] (List.map viewSongs model.doc.songs)
                    ]
                ]
            , button
                [ if model.page == 0 then
                    disabled True

                  else
                    disabled False
                , onClick PrevPage
                ]
                [ text "Previous" ]
            , text (String.fromInt (model.page + 1))
            , button
                [ if model.page >= (numPage model.doc.numDocs model.limit - 1) then
                    disabled True

                  else
                    disabled False
                , onClick NextPage
                ]
                [ text "Next" ]
            ]
        ]


viewSongs : SongModel -> Html Msg
viewSongs model =
    tr []
        [ td [] [ text model.title ]
        , td [] [ text model.artist ]
        , td [] [ text model.album ]
        , td [] [ img [ src model.image, height 200, width 200 ] [] ]
        , td []
            [ audio [ src model.preview, controls True, attribute "type" "audio/mpeg" ] []
            ]
        , td [] [ button [ onClick (DeleteSong model.id) ] [ text "Delete" ], button [ onClick (EditSong model) ] [ text "Edit" ] ]
        ]


numPage : Int -> Int -> Int
numPage total limit =
    ceiling (toFloat total / toFloat limit)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getSong : Int -> Int -> Cmd Msg
getSong skip limit =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://localhost:3000/api/song/" ++ String.fromInt skip ++ "/" ++ String.fromInt limit
        , body = Http.emptyBody
        , expect = Http.expectJson GotSongs decodeDocs
        , timeout = Nothing
        , tracker = Nothing
        }


resetGetSong : Int -> Int -> Cmd Msg
resetGetSong skip limit =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://localhost:3000/api/song/" ++ String.fromInt skip ++ "/" ++ String.fromInt limit
        , body = Http.emptyBody
        , expect = Http.expectJson ResetPage decodeDocs
        , timeout = Nothing
        , tracker = Nothing
        }


getSearchSong : Int -> Int -> String -> Cmd Msg
getSearchSong skip limit keyword =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://localhost:3000/api/song/" ++ String.fromInt skip ++ "/" ++ String.fromInt limit ++ "/" ++ keyword
        , body = Http.emptyBody
        , expect = Http.expectJson SearchedSong decodeDocs
        , timeout = Nothing
        , tracker = Nothing
        }


deleteSong : String -> Cmd Msg
deleteSong id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/api/song/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeletedSong
        , timeout = Nothing
        , tracker = Nothing
        }


updateSong : SongModel -> Cmd Msg
updateSong song =
    Http.request
        { method = "PUT"
        , headers = []
        , url =
            "http://localhost:3000/api/song/"
                ++ song.id
                ++ "/"
                ++ replaceChara song.title
                ++ "/"
                ++ replaceChara song.artist
                ++ "/"
                ++ replaceChara song.album
                ++ "/"
                ++ replaceChara song.image
                ++ "/"
                ++ replaceChara song.preview
        , body = Http.emptyBody
        , expect = Http.expectWhatever UpdatedSong
        , timeout = Nothing
        , tracker = Nothing
        }


createSong : SongModel -> Cmd Msg
createSong song =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            "http://localhost:3000/api/song/"
                ++ replaceChara song.title
                ++ "/"
                ++ replaceChara song.artist
                ++ "/"
                ++ replaceChara song.album
                ++ "/"
                ++ replaceChara song.image
                ++ "/"
                ++ replaceChara song.preview
        , body = Http.emptyBody
        , expect = Http.expectWhatever CreatedSong
        , timeout = Nothing
        , tracker = Nothing
        }


replaceChara : String -> String
replaceChara val =
    String.replace "/" "_" (String.replace " " "+" val)



-- JSON


decodeDocs : Decode.Decoder DocsModel
decodeDocs =
    Decode.map2 DocsModel
        (Decode.at [ "0", "Songs" ] (Decode.list decodeSongs))
        (Decode.at [ "1", "NumDocs" ] Decode.int)


decodeSongs : Decode.Decoder SongModel
decodeSongs =
    Decode.map6 SongModel
        (Decode.at [ "_id" ] Decode.string)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "artist", "name" ] Decode.string)
        (Decode.at [ "album", "title" ] Decode.string)
        (Decode.at [ "album", "cover_medium" ] Decode.string)
        (Decode.at [ "preview" ] Decode.string)



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
