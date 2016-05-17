{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib2 (
  f,
  f_without_show,
  f_with_eq,
  hs,
  ps,
  apiSpec_TH,
  apiSpec_String_TH
) where



import           Data.Int
import           Data.Text (Text)
import           Haskell.Interop.Prime
import           LN.T


ps :: [Mk]
ps = defaultPurescriptMks



hs :: [Mk]
hs = defaultHaskellMks



myPs :: [Mk]
myPs =
  [ MkType
  , MkLens
  , MkMk
  , MkUnwrap
  , MkEncodeJson
  , MkDecodeJson
  , MkRequestable
  , MkRespondable
  , MkIsForeign
  , MkShow
  ]


f :: forall t. t -> (t, [Mk], [Mk])
f t = (t, myPs, hs)



f_without_show :: forall t. t -> (t, [Mk], [Mk])
f_without_show t = (t, filter (/= MkShow) myPs, hs)



f_with_eq :: forall t. t -> (t, [Mk], [Mk])
f_with_eq t = (t, myPs ++ [MkEq], hs)



apiSpec_TH :: Api_TH
apiSpec_TH = Api_TH {
  apiPrefix_TH = "/api",
  apiEntries_TH = apiEntries_TH'
}



type Int64_L = [Int64]
type String_L = [String]



apiEntries_TH' :: [ApiEntry_TH]
apiEntries_TH' =
  [


  -- Empty
    ApiEntry_TH "Emptys"
    [ ParNone_TH ]
    [ ApiGET_TH ''EmptyResponses ]

  , ApiEntry_TH "Empty"
    [ ParNone_TH ]
    [ ApiPOST_TH ''EmptyRequest ''EmptyResponse ]

  , ApiEntry_TH "Empty"
    [ Par_TH [("empty_id", ''Int64)] ]
    [ ApiGET_TH ''EmptyResponse
    , ApiPUT_TH ''EmptyRequest ''EmptyResponse
    , ApiDELETE_TH ''()
    ]

  -- Api
  , ApiEntry_TH "Apis"
    [ ParNone_TH ]
    [ ApiGET_TH ''ApiResponses ]

  , ApiEntry_TH "Api"
    [ ParNone_TH ]
    [ ApiPOST_TH ''ApiRequest ''ApiResponse ]

  , ApiEntry_TH "Api"
    [ Par_TH [("api_id", ''Int64)] ]
    [ ApiGET_TH ''ApiResponse
    , ApiPUT_TH ''ApiRequest ''ApiResponse
    , ApiDELETE_TH ''()
    ]



  -- Board
  , ApiEntry_TH "Boards"
    [ ParNone_TH
    , ParBy_TH "ByBoardsIds" ''Int64_L
    , ParBy_TH "ByForumId" ''Int64
    ]
    [ ApiGET_TH ''BoardResponses ]

  , ApiEntry_TH "Board"
    [ ParNone_TH ]
    [ ApiPOST_TH ''BoardRequest ''BoardResponse ]

  , ApiEntry_TH "Board"
    [ Par_TH [("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardResponse
    , ApiPUT_TH ''BoardRequest ''BoardResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "BoardStats"
    [ ParNone_TH ]
    [ ApiGET_TH ''BoardStatResponses ]

  , ApiEntry_TH "BoardStat"
    [ Par_TH [("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardStatResponse ]



  -- Counts

  -- Users Count
  , ApiEntry_TH "UsersCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Organizations Count
  , ApiEntry_TH "OrganizationsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Teams Count
  , ApiEntry_TH "TeamsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Forums Count
  , ApiEntry_TH "ForumsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Boards Count
  , ApiEntry_TH "BoardsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Threads Count
  , ApiEntry_TH "ThreadsCount"
    [ ParNone_TH
    , ParBy_TH "ByBoardId" ''Int64
    ]
    [ ApiGET_TH ''CountResponses ]

  -- ThreadPosts Count
  , ApiEntry_TH "ThreadPostsCount"
    [ ParNone_TH
    , ParBy_TH "ByThreadId" ''Int64
    ]
    [ ApiGET_TH ''CountResponses ]

  -- Resources Count
  , ApiEntry_TH "ResourcesCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Leurons Count
  , ApiEntry_TH "LeuronsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]



  -- Forum
  , ApiEntry_TH "Forums"
    [ ParNone_TH
    , ParBy_TH "ByOrganizationName" ''Text
    , ParBy_TH "ByForumsIds" ''Int64_L
    , ParBy_TH "ByOrganizationId" ''Int64
    ]
    [ ApiGET_TH ''ForumResponses ]

  , ApiEntry_TH "Forum"
    [ ParNone_TH ]
    [ ApiPOST_TH ''ForumRequest ''ForumResponse ]

  , ApiEntry_TH "Forum"
    [ Par_TH [("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumResponse
    , ApiPUT_TH ''ForumRequest ''ForumResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ForumStats"
    [ ParNone_TH ]
    [ ApiGET_TH ''ForumStatResponses ]

  , ApiEntry_TH "ForumStat"
    [ Par_TH [("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumStatResponse ]



  -- Leuron
  , ApiEntry_TH "Leurons"
    [ ParNone_TH ]
    [ ApiGET_TH ''LeuronResponses ]

  , ApiEntry_TH "Leuron"
    [ ParNone_TH ]
    [ ApiPOST_TH ''LeuronRequest ''LeuronResponse ]

  , ApiEntry_TH "Leuron"
    [ Par_TH [("leuron_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronResponse
    , ApiPUT_TH ''LeuronRequest ''LeuronResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "LeuronLikeStats"
    [ ParBy_TH "ByLeuronsIds" ''Int64_L
--    , ParBy_TH "ByLeuronLikesIds" ''Int64_L
    ]
    [ ApiGET_TH ''LeuronLikeStatResponses ]

  , ApiEntry_TH "LeuronLikeStat"
    [ Par_TH [("leuron_like_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronLikeStatResponse ]



  -- Leuron Stars
  , ApiEntry_TH "LeuronStars"
    [ ParNone_TH
--    , ParBy_TH "ByLeuronStarsIds" ''Int64_L
    , ParBy_TH "ByLeuronsIds" ''Int64_L
    , ParBy_TH "ByLeuronId" ''Int64
    ]
    [ ApiGET_TH ''LeuronStarResponses ]

  , ApiEntry_TH "LeuronStar"
    [ ParBy_TH "ByLeuronId" ''Int64 ]
    [ ApiPOST_TH ''LeuronStarRequest ''LeuronStarResponse ]

  , ApiEntry_TH "LeuronStar"
    [ Par_TH [("leuron_star_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronStarResponse
    , ApiPUT_TH ''LeuronStarRequest ''LeuronStarResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "LeuronStarStats"
    [ ParBy_TH "ByLeuronsIds" ''Int64_L
--    , ParBy_TH "ByLeuronStarsIds" ''Int64_L
    ]
    [ ApiGET_TH ''LeuronStarStatResponses ]

  , ApiEntry_TH "LeuronStarStat"
    [ Par_TH [("leuron_star_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronStarStatResponse ]



  -- Me
  , ApiEntry_TH "Me"
    [ ParNone_TH ]
    [ ApiGET_TH ''UserResponse ]

  , ApiEntry_TH "MePack"
    [ ParNone_TH ]
    [ ApiGET_TH ''UserPackResponse ]



  -- Organization
  , ApiEntry_TH "Organizations"
    [ ParNone_TH ]
    [ ApiGET_TH ''OrganizationResponses ]

  , ApiEntry_TH "Organization"
    [ ParNone_TH ]
    [ ApiPOST_TH ''OrganizationRequest ''OrganizationResponse ]

  , ApiEntry_TH "Organization"
    [ Par_TH [("organization_id", ''Int64)] ]
    [ ApiGET_TH ''OrganizationResponse
    , ApiPUT_TH ''OrganizationRequest ''OrganizationResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "OrganizationStats"
    [ ParNone_TH ]
    [ ApiGET_TH ''OrganizationStatResponses ]

  , ApiEntry_TH "OrganizationStat"
    [ Par_TH [("organization_id", ''Int64)] ]
    [ ApiGET_TH ''OrganizationStatResponse ]



  -- Pm
  , ApiEntry_TH "Pms"
    [ ParNone_TH ]
    [ ApiGET_TH ''PmResponses ]

  , ApiEntry_TH "Pm"
    [ ParNone_TH ]
    [ ApiPOST_TH ''PmRequest ''PmResponse ]

  , ApiEntry_TH "Pm"
    [ Par_TH [("pm_id", ''Int64)] ]
    [ ApiGET_TH ''PmResponse
    , ApiPUT_TH ''PmRequest ''PmResponse
    , ApiDELETE_TH ''()
    ]



  -- PmIn
  , ApiEntry_TH "PmIns"
    [ ParNone_TH ]
    [ ApiGET_TH ''PmInResponses ]

  , ApiEntry_TH "PmIn"
    [ ParNone_TH ]
    [ ApiPOST_TH ''PmInRequest ''PmInResponse ]

  , ApiEntry_TH "PmIn"
    [ Par_TH [("pm_in_id", ''Int64)] ]
    [ ApiGET_TH ''PmInResponse
    , ApiPUT_TH ''PmInRequest ''PmInResponse
    , ApiDELETE_TH ''()
    ]



  -- PmOut
  , ApiEntry_TH "PmOuts"
    [ ParNone_TH ]
    [ ApiGET_TH ''PmOutResponses ]

  , ApiEntry_TH "PmOut"
    [ ParNone_TH ]
    [ ApiPOST_TH ''PmOutRequest ''PmOutResponse ]

  , ApiEntry_TH "PmOut"
    [ Par_TH [("pm_out_id", ''Int64)] ]
    [ ApiGET_TH ''PmOutResponse
    , ApiPUT_TH ''PmOutRequest ''PmOutResponse
    , ApiDELETE_TH ''()
    ]



  -- Resource
  , ApiEntry_TH "Resources"
    [ ParNone_TH ]
    [ ApiGET_TH ''ResourceResponses ]

  , ApiEntry_TH "Resource"
    [ ParNone_TH ]
    [ ApiPOST_TH ''ResourceRequest ''ResourceResponse ]

  , ApiEntry_TH "Resource"
    [ Par_TH [("resource_id", ''Int64)] ]
    [ ApiGET_TH ''ResourceResponse
    , ApiPUT_TH ''ResourceRequest ''ResourceResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ResourceStats"
    [ ParBy_TH "ByResourcesIds" ''Int64_L ]
    [ ApiGET_TH ''ResourceStatResponses ]

  , ApiEntry_TH "ResourceStat"
    [ Par_TH [("resource_id", ''Int64)] ]
    [ ApiGET_TH ''ResourceStatResponse ]



  -- Resource Likes
  , ApiEntry_TH "ResourceLikes"
    [ ParNone_TH
--    , ParBy_TH "ByResourceLikesIds" ''Int64_L
    , ParBy_TH "ByResourcesIds" ''Int64_L
    , ParBy_TH "ByResourceId" ''Int64
    ]
    [ ApiGET_TH ''ResourceLikeResponses ]

  , ApiEntry_TH "ResourceLike"
    [ ParBy_TH "ByResourceId" ''Int64 ]
    [ ApiPOST_TH ''ResourceLikeRequest ''ResourceLikeResponse ]

  , ApiEntry_TH "ResourceLike"
    [ Par_TH [("resource_like_id", ''Int64)] ]
    [ ApiGET_TH ''ResourceLikeResponse
    , ApiPUT_TH ''ResourceLikeRequest ''ResourceLikeResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ResourceLikeStats"
    [ ParBy_TH "ByResourcesIds" ''Int64_L
--    , ParBy_TH "ByResourceLikesIds" ''Int64_L
    ]
    [ ApiGET_TH ''ResourceLikeStatResponses ]

  , ApiEntry_TH "ResourceLikeStat"
    [ Par_TH [("resource_like_id", ''Int64)] ]
    [ ApiGET_TH ''ResourceLikeStatResponse ]



  -- Resource Stars
  , ApiEntry_TH "ResourceStars"
    [ ParNone_TH
--    , ParBy_TH "ByResourceStarsIds" ''Int64_L
    , ParBy_TH "ByResourcesIds" ''Int64_L
    , ParBy_TH "ByResourceId" ''Int64
    ]
    [ ApiGET_TH ''ResourceStarResponses ]

  , ApiEntry_TH "ResourceStar"
    [ ParBy_TH "ByResourceId" ''Int64 ]
    [ ApiPOST_TH ''ResourceStarRequest ''ResourceStarResponse ]

  , ApiEntry_TH "ResourceStar"
    [ Par_TH [("resource_star_id", ''Int64)] ]
    [ ApiGET_TH ''ResourceStarResponse
    , ApiPUT_TH ''ResourceStarRequest ''ResourceStarResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ResourceStarStats"
    [ ParBy_TH "ByResourcesIds" ''Int64_L
--    , ParBy_TH "ByResourceStarsIds" ''Int64_L
    ]
    [ ApiGET_TH ''ResourceStarStatResponses ]

  , ApiEntry_TH "ResourceStarStat"
    [ Par_TH [("resource_star_id", ''Int64)] ]
    [ ApiGET_TH ''ResourceStarStatResponse ]



  -- Team
  , ApiEntry_TH "Teams"
    [ ParNone_TH ]
    [ ApiGET_TH ''TeamResponses ]

  , ApiEntry_TH "Team"
    [ ParNone_TH ]
    [ ApiPOST_TH ''TeamRequest ''TeamResponse ]

  , ApiEntry_TH "Team"
    [ Par_TH [("team_id", ''Int64)] ]
    [ ApiGET_TH ''TeamResponse
    , ApiPUT_TH ''TeamRequest ''TeamResponse
    , ApiDELETE_TH ''()
    ]



  -- Thread
  , ApiEntry_TH "Threads"
    [ ParNone_TH ]
    [ ApiGET_TH ''ThreadResponses ]

  , ApiEntry_TH "Thread"
    [ ParBy_TH "ByBoardId" ''Int64 ]
    [ ApiPOST_TH ''ThreadRequest ''ThreadResponse ]

  , ApiEntry_TH "Thread"
    [ Par_TH [("thread_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadResponse
    , ApiPUT_TH ''ThreadRequest ''ThreadResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ThreadStats"
    [ ParNone_TH ]
    [ ApiGET_TH ''ThreadStatResponses ]

  , ApiEntry_TH "ThreadStat"
    [ Par_TH [("thread_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadStatResponse ]



  -- ThreadPost
  , ApiEntry_TH "ThreadPosts"
    [ ParNone_TH
    , ParBy_TH "ByThreadId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPostResponses ]

  , ApiEntry_TH "ThreadPost"
    [ ParBy_TH "ByThreadId" ''Int64 ]
    [ ApiPOST_TH ''ThreadPostRequest ''ThreadPostResponse ]

  , ApiEntry_TH "ThreadPost"
    [ Par_TH [("thread_post_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostResponse
    , ApiPUT_TH ''ThreadPostRequest ''ThreadPostResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ThreadPostStats"
    [ ParBy_TH "ByThreadPostsIds" ''Int64_L ]
    [ ApiGET_TH ''ThreadPostStatResponses ]

  , ApiEntry_TH "ThreadPostStat"
    [ Par_TH [("thread_post_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostStatResponse ]



  -- Thread Post Likes
  , ApiEntry_TH "ThreadPostLikes"
    [ ParNone_TH
    , ParBy_TH "ByThreadPostLikesIds" ''Int64_L
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPostLikeResponses ]

  , ApiEntry_TH "ThreadPostLike"
    [ ParBy_TH "ByThreadPostId" ''Int64 ]
    [ ApiPOST_TH ''ThreadPostLikeRequest ''ThreadPostLikeResponse ]

  , ApiEntry_TH "ThreadPostLike"
    [ Par_TH [("thread_post_like_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostLikeResponse
    , ApiPUT_TH ''ThreadPostLikeRequest ''ThreadPostLikeResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ThreadPostLikeStats"
    [ ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostLikesIds" ''Int64_L
    ]
    [ ApiGET_TH ''ThreadPostLikeStatResponses ]

  , ApiEntry_TH "ThreadPostLikeStat"
    [ Par_TH [("thread_post_like_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostLikeStatResponse ]



  -- Thread Post Stars
  , ApiEntry_TH "ThreadPostStars"
    [ ParNone_TH
    , ParBy_TH "ByThreadPostStarsIds" ''Int64_L
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPostStarResponses ]

  , ApiEntry_TH "ThreadPostStar"
    [ ParBy_TH "ByThreadPostId" ''Int64 ]
    [ ApiPOST_TH ''ThreadPostStarRequest ''ThreadPostStarResponse ]

  , ApiEntry_TH "ThreadPostStar"
    [ Par_TH [("thread_post_star_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostStarResponse
    , ApiPUT_TH ''ThreadPostStarRequest ''ThreadPostStarResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ThreadPostStarStats"
    [ ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostStarsIds" ''Int64_L
    ]
    [ ApiGET_TH ''ThreadPostStarStatResponses ]

  , ApiEntry_TH "ThreadPostStarStat"
    [ Par_TH [("thread_post_star_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostStarStatResponse ]



  -- User
  , ApiEntry_TH "Users"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByUsersNicks" ''String_L
    ]
    [ ApiGET_TH ''UserResponses ]

  , ApiEntry_TH "User"
    [ ParNone_TH ]
    [ ApiPOST_TH ''UserRequest ''UserResponse ]

  , ApiEntry_TH "User"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserResponse
    , ApiPUT_TH ''UserRequest ''UserResponse
    , ApiDELETE_TH ''()
    ]



  -- User Sanitized
  , ApiEntry_TH "UsersSanitized"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByUsersNicks" ''String_L
    ]
    [ ApiGET_TH ''UserSanitizedResponses ]

  , ApiEntry_TH "UserSanitized"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserSanitizedResponse ]

  , ApiEntry_TH "UserSanitizedStats"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    ]
    [ ApiGET_TH ''UserSanitizedStatResponse ]

  , ApiEntry_TH "UserSanitizedStat"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserSanitizedStatResponse ]



  --
  -- Packs
  --



  -- Packs: Organization
  , ApiEntry_TH "OrganizationPacks"
    [ ParNone_TH
    , ParBy_TH "ByOrganizationsIds" ''Int64_L
    ]
    [ ApiGET_TH ''OrganizationPackResponses ]

  , ApiEntry_TH "OrganizationPack"
    [ Par_TH [("organization_id", ''Int64)] ]
    [ ApiGET_TH ''OrganizationPackResponse ]



  , ApiEntry_TH "TeamPacks"
    [ ParNone_TH
    ]
    [ ApiGET_TH ''TeamPackResponses ]

  , ApiEntry_TH "TeamPack"
    [ Par_TH [("team_id", ''Int64)] ]
    [ ApiGET_TH ''TeamPackResponse ]



  -- Packs: User
  , ApiEntry_TH "UserPacks"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    ]
    [ ApiGET_TH ''UserPackResponses ]

  , ApiEntry_TH "UserPack"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserPackResponse ]



  -- Packs: UserSanitized
  , ApiEntry_TH "UserSanitizedPacks"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    ]
    [ ApiGET_TH ''UserSanitizedPackResponses ]

  , ApiEntry_TH "UserSanitizedPack"
    [ Par_TH [("user_id", ''Int64)] ]
    [ ApiGET_TH ''UserSanitizedPackResponse ]



  -- Packs: Forum
  , ApiEntry_TH "ForumPacks"
    [ ParNone_TH
    , ParBy_TH "ByForumId" ''Int64
    , ParBy_TH "ByForumsIds" ''Int64_L
    ]
    [ ApiGET_TH ''ForumPackResponses ]

  , ApiEntry_TH "ForumPack"
    [ Par_TH [("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumPackResponse ]



  -- Packs: Board
  , ApiEntry_TH "BoardPacks"
    [ ParNone_TH
    , ParBy_TH "ByForumId" ''Int64
    , ParBy_TH "ByBoardsIds" ''Int64_L
    ]
    [ ApiGET_TH ''BoardPackResponses ]

  , ApiEntry_TH "BoardPack"
    [ Par_TH [("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardPackResponse ]



  -- Packs: Thread
  , ApiEntry_TH "ThreadPacks"
    [ ParNone_TH
    , ParBy_TH "ByThreadsIds" ''Int64_L
    , ParBy_TH "ByBoardId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPackResponses ]

  , ApiEntry_TH "ThreadPack"
    [ Par_TH [("thread_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPackResponse ]



  -- Packs: ThreadPost
  , ApiEntry_TH "ThreadPostPacks"
    [ ParNone_TH
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPostPackResponses ]

  , ApiEntry_TH "ThreadPostPack"
    [ Par_TH [("thread_post_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostPackResponse ]



  -- Packs: Resources
  , ApiEntry_TH "ResourcePacks"
    [ ParNone_TH
    , ParBy_TH "ByResourcesIds" ''Int64_L
    ]
    [ ApiGET_TH ''ResourcePackResponses ]

  , ApiEntry_TH "ResourcePack"
    [ Par_TH [("resource_id", ''Int64)] ]
    [ ApiGET_TH ''ResourcePackResponse ]



  -- Packs: Leurons
  , ApiEntry_TH "LeuronPacks"
    [ ParNone_TH
    , ParBy_TH "ByLeuronsIds" ''Int64_L
    , ParBy_TH "ByResourceId" ''Int64
    ]
    [ ApiGET_TH ''LeuronPackResponses ]

  , ApiEntry_TH "LeuronPack"
    [ Par_TH [("leuron_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronPackResponse ]



  ]







apiSpec_String_TH :: Api_TH
apiSpec_String_TH = Api_TH {
  apiPrefix_TH = "/api",
  apiEntries_TH = apiEntries_String_TH'
}



apiEntries_String_TH' :: [ApiEntry_TH]
apiEntries_String_TH' =
  [

  -- Packs: UserSanitized
    ApiEntry_TH "UserSanitizedPack"
    [ Par_TH [("user_nick", ''Text)] ]
    [ ApiGET_TH ''UserSanitizedPackResponse ]

  , ApiEntry_TH "Organization"
    [ Par_TH [("organization_name", ''Text)] ]
    [ ApiGET_TH ''OrganizationResponse ]

  , ApiEntry_TH "Forum"
    [ ParBoth_TH [("forum_name", ''Text)] ("ByOrganizationName", ''Text) ]
    [ ApiGET_TH ''ForumResponse ]

  , ApiEntry_TH "Board"
    [ ParBoth_TH [("board_name", ''Text)] ("ByForumId", ''Int64) ]
    [ ApiGET_TH ''BoardResponse ]

  , ApiEntry_TH "Thread"
    [ ParBoth_TH [("thread_name", ''Text)] ("ByBoardId", ''Int64) ]
    [ ApiGET_TH ''ThreadResponse ]

  ]
