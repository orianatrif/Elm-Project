module View.Posts exposing (..)


import Html.Attributes exposing (class, href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Html exposing (Html, div, table, th, tr, td, text, a,  thead, tbody, p)
import Time exposing (Posix)
import Maybe exposing (Maybe(..), map, withDefault)
import Util.Time exposing (durationBetween)
import Html.Attributes exposing (for)
import Html.Attributes exposing (id, value, selected, type_, checked)
import Html.Events exposing (onInput, onCheck)
import Html exposing (Html, div, label, text, input, select, option)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable postsConfig currentTime posts =
     div [] [
       table [] [
         thead [] [
           tr [] [
             th [] [text "Score"],
             th [] [text "Title"],
             th [] [text "Type"],
             th [] [text "Posted Date"],
             th [] [text "Link"]
           ]
         ],
         tbody [] (createPostsRows posts currentTime postsConfig)
       ]
     ]

createPostsRows : List Post -> Time.Posix -> PostsConfig -> List (Html Msg)
createPostsRows posts currentTime postsConfig=
    posts
        |> filterPosts postsConfig
        |> List.map (postRow currentTime)


postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        relativeDuration =
            Util.Time.durationBetween post.time currentTime
                |> Maybe.map Util.Time.formatDuration
                |> Maybe.withDefault "just now"
    in
        tr [] [
            td [class "post-score"] [text (String.fromInt post.score)],
            td [class "post-title"] [text post.title],
            td [class "post-type"] [text post.type_],
            td [class "post-time"] [text (Util.Time.formatTime Time.utc post.time ++ " (" ++ relativeDuration ++ ")")],
            td [class "post-url"] [maybeLink post.url]
        ]

maybeLink : Maybe String -> Html Msg
maybeLink maybeUrl =
    case maybeUrl of
        Just value -> a [href value, class "post-url-link"] [text "Link"]
        Nothing -> text ""


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    let
        postsToShowOptions = [10, 25, 50]
        sortByOptions = ["Score", "Title", "Posted", "None"]

        postsToShowOptionView number =
            let isSelected = number == config.postsToShow in
            option [ value <| String.fromInt number, selected isSelected ] [ text <| String.fromInt number ]

        sortByOptionView sortByString =
            let isSelected = sortToString config.sortBy == sortByString in
            option [ value sortByString, selected isSelected ] [ text sortByString ]
    in
    div []
        [ div []
            [ p [] [ text "Posts to show" ]
            , select [ id "select-posts-per-page"
                     , onInput (String.toInt >> Maybe.withDefault 0 >> UpdatePostsToShow >> ConfigChanged) ]
                (List.map postsToShowOptionView postsToShowOptions)
            ]
        , div []
            [ p [] [ text "Choose sorting method" ]
            , select [ id "select-sort-by"
                     , onInput (sortFromString >> Maybe.withDefault None >> UpdateSortBy >> ConfigChanged) ]
                (List.map sortByOptionView sortByOptions)
            ]
        , div []
            [ p [] [ text "Show job posts" ]
            , input [ type_ "checkbox"
                    , id "checkbox-show-job-posts"
                    , checked config.showJobs
                    , onCheck (UpdateShowJobs >> ConfigChanged)
                    ] []
            ]
        , div []
            [ p [] [ text "Show text only posts" ]
            , input [ type_ "checkbox"
                    , checked config.showTextOnly
                    , onCheck (UpdateShowTextOnly >> ConfigChanged)
                    , id "checkbox-show-text-only-posts"
                    ] []
            ]
        ]
