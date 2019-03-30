{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module LibForum2 (
  f,
  f_withPs,
  f_withHs,
  f_withBoth,
  f_withBoth',
  f_mkType,
  f_mkType',
  myPs_MkGs,
  myHs_MkGs,
  myPs_Mks,
  myHs_Mks,
  myHs_Mks',
  apiSpec_TH,
  apiSpec_String_TH,
  haskellApiImports
) where



import           Data.Int                   (Int64)
import           Data.List                  (intercalate)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Haskell.Interop.Prime
import           Language.Haskell.TH.Syntax
import           Prelude

import           LN.T



myPs_MkGs :: [String] -> String -> [MkG]
myPs_MkGs imports header =
  [ MkGPurescriptImports
  , MkGHeader $ intercalate "\n" $ map ((<>) "import ") imports
  , MkGHeader header
  , MkGFooter "-- footer"
  ]



myHs_MkGs :: [String] -> String -> [MkG]
myHs_MkGs imports header =
  [ MkGHaskellImports
  , MkGHeader $ intercalate "\n" $ map ((<>) "import ") imports
  , MkGHeader header
  , MkGFooter "-- footer"
  ]



myPs_Mks :: [Mk]
myPs_Mks =
  [ MkType
  , MkTypeRows "R"
  , MkLens
  , MkMk
  , MkUnwrap
  , MkEncodeJson
  , MkDecodeJson
  , MkRequestable
  , MkRespondable
  -- , MkDecode
  ]



myHs_Mks :: [Mk]
myHs_Mks = myHs_Mks' []



myHs_Mks' :: [MkTypeOpts] -> [Mk]
myHs_Mks' type_opts =
  [ MkTypeWith $ [MkTypeOpts_StrictFields, MkTypeOpts_Deriving Deriving_Generic, MkTypeOpts_Deriving Deriving_Typeable, MkTypeOpts_Deriving Deriving_NFData] <> type_opts
  , MkFromJSON
  , MkToJSON
  , MkEq
  , MkShow
  ]



f :: forall t. t -> (t, [Mk], [Mk])
f t = (t, myPs_Mks, myHs_Mks)



f_withPs :: forall t. t -> [Mk] -> (t, [Mk], [Mk])
f_withPs t with = (t, myPs_Mks <> with, myHs_Mks)



f_withHs :: forall t. t -> [Mk] -> (t, [Mk], [Mk])
f_withHs t with = (t, myPs_Mks, myHs_Mks <> with)



f_withBoth :: forall t. t -> [Mk] -> (t, [Mk], [Mk])
f_withBoth t with = (t, myPs_Mks <> with, myHs_Mks <> with)



f_withBoth' :: forall t. t -> [MkTypeOpts] -> [Mk] -> (t, [Mk], [Mk])
f_withBoth' t type_opts with = (t, myPs_Mks <> with, myHs_Mks' type_opts <> with)



f_mkType
  :: String
  -> FilePath
  -> [(Name, [Mk], [Mk])]
  -> Q [Dec]

f_mkType = f_mkType' []



f_mkType'
  :: [String]
  -> String
  -> FilePath
  -> [(Name, [Mk], [Mk])]
  -> Q [Dec]

f_mkType' imports module_name module_path types =
  mkExports
    (Options
      (defaultOptionsCleanPurescript $ "../purescript-lnforum-types/src/" <> module_path <> ".purs")
      (MkGHeader "import Purescript.Api.Helpers\n" : (myPs_MkGs imports $ "module " <> module_name <> " where"))
      (defaultOptions_Haskell_adarqui $ "../lnforum-types-gen/src/" <> module_path <> ".hs")
      (myHs_MkGs imports $ tplTestHeader module_name))
    types



apiSpec_TH :: Api_TH
apiSpec_TH = Api_TH {
  apiPrefix_TH = "/api",
  apiEntries_TH = apiEntries_TH'
}



type Int64_L = [Int64]
type Text_L = [Text]



apiEntries_TH' :: [ApiEntry_TH]
apiEntries_TH' =

  map (\s -> ApiEntry_TH (s <> "Count")
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ])
    ["Boards", "Threads", "ThreadPosts"
    ,"Likes"
    ,"Users"]


  <>

  [

  -- Api
    ApiEntry_TH "Apis"
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



  -- Me
  , ApiEntry_TH "Me"
    [ ParNone_TH ]
    [ ApiGET_TH ''UserResponse ]

  , ApiEntry_TH "MePack"
    [ ParNone_TH ]
    [ ApiGET_TH ''UserPackResponse ]



  -- Forum
  , ApiEntry_Name_TH "Forums" (Just "Forum")
    [ ParNone_TH ]
    [ ApiPOST_TH ''ForumRequest ''ForumResponse ]

  , ApiEntry_Name_TH "Forums" (Just "Forum")
    [ Par_TH [("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumResponse
    , ApiPUT_TH ''ForumRequest ''ForumResponse
    ]
  , ApiEntry_TH "ForumStat"
    [ Par_TH [("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumStatResponse ]




  -- Board
  , ApiEntry_TH "Boards"
    [ ParNone_TH ]
    [ ApiGET_TH ''BoardResponses ]

  , ApiEntry_Name_TH "Boards" (Just "Board")
    [ ParNone_TH
    , ParBy_TH "ByForumId" ''Int64
    , ParBy_TH "ByBoardId" ''Int64
    ]
    [ ApiPOST_TH ''BoardRequest ''BoardResponse ]

  , ApiEntry_Name_TH "Boards" (Just "Board")
    [ Par_TH [("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardResponse
    , ApiPUT_TH ''BoardRequest ''BoardResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "BoardStats"
    [ ParBy_TH "ByBoardsIds" ''Int64_L ]
    [ ApiGET_TH ''BoardStatResponses ]

  , ApiEntry_TH "BoardStat"
    [ Par_TH [("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardStatResponse ]




  -- Threads
  , ApiEntry_TH "Threads"
    [ ParNone_TH ]
    [ ApiGET_TH ''ThreadResponses ]

  , ApiEntry_Name_TH "Threads" (Just "Thread")
    [ ParNone_TH
    , ParBy_TH "ByBoardId" ''Int64
    ]
    [ ApiPOST_TH ''ThreadRequest ''ThreadResponse ]

  , ApiEntry_Name_TH "Threads" (Just "Thread")
    [ Par_TH [("thread_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadResponse
    , ApiPUT_TH ''ThreadRequest ''ThreadResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ThreadStats"
    [ ParBy_TH "ByThreadsIds" ''Int64_L ]
    [ ApiGET_TH ''ThreadStatResponses ]

  , ApiEntry_TH "ThreadStat"
    [ Par_TH [("thread_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadStatResponse ]

  , ApiEntry_TH "ThreadsCount"
    [ ParBy_TH "ByBoardId" ''Int64 ]
    [ ApiGET_TH ''CountResponses ]



  --- ThreadPosts
  , ApiEntry_TH "ThreadPosts"
    [ ParNone_TH ]
    [ ApiGET_TH ''ThreadPostResponses ]

  , ApiEntry_Name_TH "ThreadPosts" (Just "ThreadPost")
    [ ParNone_TH
    , ParBy_TH "ByThreadId" ''Int64
    ]
    [ ApiPOST_TH ''ThreadPostRequest ''ThreadPostResponse ]

  , ApiEntry_Name_TH "ThreadPosts" (Just "ThreadPost")
    [ Par_TH [("thread_post_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostResponse
    , ApiPUT_TH ''ThreadPostRequest ''ThreadPostResponse
    , ApiDELETE_TH ''()
    ]

  , ApiEntry_TH "ThreadPostStats"
    [ ParBy_TH "ByThreadPostsIds" ''Int64_L ]
    [ ApiGET_TH ''ThreadPostStatResponses ]

  , ApiEntry_TH "ThreadPostStat"
    [ Par_TH [("threadpost_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostStatResponse ]

  , ApiEntry_TH "ThreadPostsCount"
    [ ParBy_TH "ByBoardId" ''Int64 ]
    [ ApiGET_TH ''CountResponses ]

  , ApiEntry_TH "ThreadPostsCount"
    [ ParBy_TH "ByThreadId" ''Int64 ]
    [ ApiGET_TH ''CountResponses ]



  -- Likes
  , ApiEntry_TH "Likes"
    [ ParNone_TH ]
    [ ApiGET_TH ''LikeResponses ]

  -- TODO FIXME: something is forcing it to postLike, when the only postLike should be postLike_ByThreadPostId
  , ApiEntry_TH "Likes"
    [ ParNone_TH
    , ParBy_TH "ByThreadPostId" ''Int64
    ]
    [ ApiPOST_TH ''LikeRequest ''LikeResponse ]

  , ApiEntry_Name_TH "Likes" (Just "Like")
    [ Par_TH [("like_id", ''Int64)] ]
    [ ApiGET_TH ''LikeResponse
    , ApiPUT_TH ''LikeRequest ''LikeResponse
    , ApiDELETE_TH ''()
    ]



  -- User
  , ApiEntry_TH "Users"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByUsersNames" ''Text_L
    ]
    [ ApiGET_TH ''UserResponses ]

  , ApiEntry_TH "Users"
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
    , ParBy_TH "ByUserId" ''Int64
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
    , ParBy_TH "ByUsersNames" ''Text_L
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



  -- -- Packs: Boot
  -- , ApiEntry_TH "BootPack"
  --   [ ParNone_TH ]
  --   [ ApiGET_TH ''BootPackResponse ]



  -- Packs: User
  , ApiEntry_TH "UserPacks"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByEmail" ''Text
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



  -- Packs: Forums
  , ApiEntry_TH "ForumPack"
    [ Par_TH [("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumPackResponse ]



  -- Packs: Boards
  , ApiEntry_TH "BoardPacks"
    [ ParNone_TH
    , ParBy_TH "ByBoardsIds" ''Int64_L
    , ParBy_TH "ByForumId" ''Int64
    ]
    [ ApiGET_TH ''BoardPackResponses ]

  , ApiEntry_TH "BoardPack"
    [ Par_TH [("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardPackResponse ]



  -- Packs: Threads
  , ApiEntry_TH "ThreadPacks"
    [ ParNone_TH
    , ParBy_TH "ByThreadsIds" ''Int64_L
    , ParBy_TH "ByBoardId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPackResponses ]

  , ApiEntry_TH "ThreadPack"
    [ Par_TH [("thread_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPackResponse ]



  -- Packs: ThreadPosts
  , ApiEntry_TH "ThreadPostPacks"
    [ ParNone_TH
    , ParBy_TH "ByThreadPostsIds" ''Int64_L
    , ParBy_TH "ByThreadId" ''Int64
    ]
    [ ApiGET_TH ''ThreadPostPackResponses ]

  , ApiEntry_TH "ThreadPostPack"
    [ Par_TH [("thread_post_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPostPackResponse ]

  , ApiEntry_TH "RecentThreadPostPacks"
    [ ParNone_TH ]
    [ ApiGET_TH ''ThreadPostPackResponses ]

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
    [ Par_TH [("user_name", ''Text)] ]
    [ ApiGET_TH ''UserSanitizedPackResponse ]

  -- Packs: Forum
  , ApiEntry_TH "ForumPack"
    [ Par_TH [("forum_sid", ''Text)] ] --  ("forum_id", ''Int64)] ]
    [ ApiGET_TH ''ForumPackResponse ]

  -- Packs: Board
  , ApiEntry_TH "BoardPack"
    [ Par_TH [("board_sid", ''Text)] ] --  ("board_id", ''Int64)] ]
    [ ApiGET_TH ''BoardPackResponse ]

  -- Packs: Thread
  , ApiEntry_TH "ThreadPack"
    [ Par_TH [("thread_sid", ''Text)] ] --  ("board_id", ''Int64)] ]
    [ ApiGET_TH ''ThreadPackResponse ]
  ]




haskellApiImports :: String
haskellApiImports =
  intercalate "\n" $
  [ ""
  , "import LN.T"
  , ""
  ]
