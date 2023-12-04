module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None

sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}

sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" -> Just Score
        "Title" -> Just Title
        "Posted" -> Just Posted
        "None" -> Just None
        _ -> Nothing
   -- Debug.todo "sortFromString"


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }



defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = UpdatePostsToFetch Int
    | UpdatePostsToShow Int
    | UpdateSortBy SortBy
    | UpdateShowJobs Bool
    | UpdateShowTextOnly Bool


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change postsConfig =
    case change of
        UpdatePostsToFetch newValue -> { postsConfig | postsToFetch = newValue }
        UpdatePostsToShow newValue -> { postsConfig | postsToShow = newValue }
        UpdateSortBy newSort -> { postsConfig | sortBy = newSort }
        UpdateShowJobs newBool -> { postsConfig | showJobs = newBool }
        UpdateShowTextOnly newBool -> { postsConfig | showTextOnly = newBool }


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts: PostsConfig -> List Post -> List Post 
filterPosts postsConfig posts  =     
    let
        filteredPosts =
            if not postsConfig.showJobs then
                
                List.filter (\post -> post.type_ /= "job") posts
            else
                posts
        filteredPosts2 =
            if postsConfig.showTextOnly then
                filteredPosts
            else
                List.filter (\post -> 
                    case post.url of
                        Just _ -> True
                        Nothing -> False
                ) filteredPosts

        filteredPosts3 =
            List.take postsConfig.postsToShow filteredPosts2

        filteredPosts4 =
            List.sortWith (sortToCompareFn postsConfig.sortBy) filteredPosts3
    in
        filteredPosts4

    
    -- posts
    --     |> List.filter (\post -> if (not postsConfig.showJobs) && post.type_ == "job" then
    --                                 False
    --                             else
    --                                 True
    --     )
    --     |> List.filter (\post -> if postsConfig.showTextOnly then
    --                             True
    --                             else 
    --                                 case post.url of
    --                                     Just _ -> True
    --                                     Nothing -> False
    --     )
    --     |> List.take postsConfig.postsToShow
    --     |> List.sortWith (sortToCompareFn postsConfig.sortBy)  

    --      let
    --     filteredPosts =
    --         if not postsConfig.showJobs then
    --             posts
    --         else
    --             List.filter (\post -> post.type_ /= "job") posts

    --     filteredPosts2 =
    --         if postsConfig.showTextOnly then
    --             filteredPosts
    --         else
    --             List.filter (\post -> case post.url of
    --                                     Just _ -> True
    --                                     Nothing -> False
    --                         ) filteredPosts
    --     filteredPosts3 =
    --         List.take postsConfig.postsToShow filteredPosts2
    --     filteredPosts4 =
    --         List.sortWith (sortToCompareFn postsConfig.sortBy) filteredPosts3

    -- in
    --     List.sortWith (sortToCompareFn postsConfig.sortBy) filteredPosts4

--i receive 28 points but when i added the above code i receiove 31 without runinng grade
-- filteredPosts3 =
--             List.take postsConfig.postsToShow filteredPosts2


