{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib2 (
  f,
  f_without_show,
  f_with_eq,
  hs,
  ps,
  apiSpec_TH,
  apiSpec_String_TH,
  haskellApiImports
) where



import           Data.Int
import           Data.List
-- import           Data.String (String)
import           Haskell.Interop.Prime
import           LN.T


ps :: [Mk]
ps = defaultPurescriptMks



hs :: [Mk]
hs = defaultHaskellMks



myPs :: [Mk]
myPs =
  [ MkType
  , MkTypeRows "R"
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
    , ParBy_TH "ByOrganizationName" ''String
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
    [ ParBy_TH "ByResourceId" ''Int64]
    [ ApiPOST_TH ''LeuronRequest ''LeuronResponse ]

  , ApiEntry_TH "Leuron"
    [ Par_TH [("leuron_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronResponse
    , ApiPUT_TH ''LeuronRequest ''LeuronResponse
    , ApiDELETE_TH ''()
    ]



  -- Likes
  , ApiEntry_TH "Likes"
    [ ParNone_TH
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostId" ''Int64
    , ParBy_TH "ByResourceId" ''Int64
    ]
    [ ApiGET_TH ''LikeResponses ]

  , ApiEntry_TH "Like"
    [ ParBy_TH "ByThreadPostId" ''Int64
    , ParBy_TH "ByLeuronId" ''Int64
    ]
    [ ApiPOST_TH ''LikeRequest ''LikeResponse ]

  , ApiEntry_TH "Like"
    [ Par_TH [("like_id", ''Int64)] ]
    [ ApiGET_TH ''LikeResponse
    , ApiPUT_TH ''LikeRequest ''LikeResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "LikeStats"
    [ ParBy_TH "ByThreadPostsIds" ''Int64_L
    ]
    [ ApiGET_TH ''LikeStatResponses ]

  , ApiEntry_TH "LikeStat"
    [ Par_TH [("like_id", ''Int64)] ]
    [ ApiGET_TH ''LikeStatResponse ]



  -- Stars
  , ApiEntry_TH "Stars"
    [ ParNone_TH
    , ParBy_TH "ByOrganizationId" ''Int64
    , ParBy_TH "ByUserId" ''Int64
    , ParBy_TH "ByBoardId" ''Int64
    , ParBy_TH "ByThreadId" ''Int64
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostId" ''Int64
    , ParBy_TH "ByResourceId" ''Int64
    , ParBy_TH "ByLeuronId" ''Int64
    ]
    [ ApiGET_TH ''StarResponses ]

  , ApiEntry_TH "Star"
    [ ParBy_TH "ByOrganizationId" ''Int64
    , ParBy_TH "ByUserId" ''Int64
    , ParBy_TH "ByBoardId" ''Int64
    , ParBy_TH "ByThreadId" ''Int64
    , ParBy_TH "ByThreadPostId" ''Int64
    , ParBy_TH "ByResourceId" ''Int64
    , ParBy_TH "ByLeuronId" ''Int64
    ]
    [ ApiPOST_TH ''StarRequest ''StarResponse ]

  , ApiEntry_TH "Star"
    [ Par_TH [("star_id", ''Int64)] ]
    [ ApiGET_TH ''StarResponse
    , ApiPUT_TH ''StarRequest ''StarResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "StarStats"
    [ ParBy_TH "ByOrganizationId" ''Int64
    , ParBy_TH "ByUserId" ''Int64
    , ParBy_TH "ByBoardId" ''Int64
    , ParBy_TH "ByThreadId" ''Int64
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadPostId" ''Int64
    , ParBy_TH "ByResourceId" ''Int64
    , ParBy_TH "ByLeuronId" ''Int64
    ]
    [ ApiGET_TH ''StarStatResponses ]

  , ApiEntry_TH "StarStat"
    [ Par_TH [("star_id", ''Int64)] ]
    [ ApiGET_TH ''StarStatResponse ]




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
    [ ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByUserId" ''Int64
    ]
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



  -- User Profile
  , ApiEntry_TH "UserProfiles"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    ]
    [ ApiGET_TH ''ProfileResponses ]

  , ApiEntry_TH "UserProfile"
    [ Par_TH [("profile_id", ''Int64)] ]
    [ ApiGET_TH ''ProfileResponse
    , ApiPUT_TH ''ProfileRequest ''ProfileResponse
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
    , ParBy_TH "ByOrganizationId" ''Int64
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
    , ParBy_TH "ByOrganizationId" ''Int64
    , ParBy_TH "ByOrganizationName" ''String
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
    , ParBy_TH "ByBoardId" ''Int64
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
    [ Par_TH [("user_nick", ''String)] ]
    [ ApiGET_TH ''UserSanitizedPackResponse ]

  , ApiEntry_TH "Organization"
    [ Par_TH [("organization_name", ''String)] ]
    [ ApiGET_TH ''OrganizationResponse ]

  , ApiEntry_TH "OrganizationPack"
    [ Par_TH [("organization_name", ''String)] ]
    [ ApiGET_TH ''OrganizationPackResponse ]

  , ApiEntry_TH "Forum"
    [ ParBoth_TH [("forum_name", ''String)] ("ByOrganizationName", ''String) ]
    [ ApiGET_TH ''ForumResponse ]

  , ApiEntry_TH "ForumPack"
    [ ParBoth_TH [("forum_name", ''String)] ("ByOrganizationName", ''String) ]
    [ ApiGET_TH ''ForumPackResponse ]

  , ApiEntry_TH "Board"
    [ ParBoth_TH [("board_name", ''String)] ("ByForumId", ''Int64) ]
    [ ApiGET_TH ''BoardResponse ]

  , ApiEntry_TH "BoardPack"
    [ ParBoth_TH [("board_name", ''String)] ("ByForumId", ''Int64) ]
    [ ApiGET_TH ''BoardPackResponse ]

  , ApiEntry_TH "Thread"
    [ ParBoth_TH [("thread_name", ''String)] ("ByBoardId", ''Int64) ]
    [ ApiGET_TH ''ThreadResponse ]

  , ApiEntry_TH "ThreadPack"
    [ ParBoth_TH [("thread_name", ''String)] ("ByBoardId", ''Int64) ]
    [ ApiGET_TH ''ThreadPackResponse ]

  ]




haskellApiImports :: String
haskellApiImports =
  intercalate "\n" $
  ["import Data.Int"
  , "import LN.T hiding (Param(..), QueryParam, SortOrderBy(..), OrderBy(..), ParamTag(..))"
  , "import LN.T.Param.String"
  , ""
  , ""
  ]
