{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module LibNotes2 (
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
  , MkIsForeign
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
      (defaultOptionsCleanPurescript $ "../purescript-lnotes-types/src/" <> module_path <> ".purs")
      (MkGHeader "import Purescript.Api.Helpers\n" : (myPs_MkGs imports $ "module " <> module_name <> " where"))
      (defaultOptions_Haskell_adarqui $ "../haskell-lnotes-types/src/" <> module_path <> ".hs")
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



  -- Counts

  -- Users Count
  , ApiEntry_TH "UsersCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Resources Count
  , ApiEntry_TH "ResourcesCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Leurons Count
  , ApiEntry_TH "LeuronsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Leurons Count
  , ApiEntry_TH "LeuronNodesCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Leurons Count
  , ApiEntry_TH "LeuronTrainingCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- Buckets Count
  , ApiEntry_TH "BucketsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- BucketRounds Count
  , ApiEntry_TH "BucketRoundsCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]

  -- BucketNodes Count
  , ApiEntry_TH "BucketNodesCount"
    [ ParNone_TH ]
    [ ApiGET_TH ''CountResponses ]


  -- Leuron
  , ApiEntry_TH "Leurons"
    [ ParNone_TH ]
    [ ApiGET_TH ''LeuronResponses ]

  , ApiEntry_Name_TH "Leurons" (Just "Leuron")
    [ ParBy_TH "ByResourceId" ''Int64]
    [ ApiPOST_TH ''LeuronRequest ''LeuronResponse ]

  , ApiEntry_Name_TH "Leurons" (Just "Leuron")
    [ Par_TH [("leuron_id", ''Int64)] ]
    [ ApiGET_TH ''LeuronResponse
    , ApiPUT_TH ''LeuronRequest ''LeuronResponse
    , ApiDELETE_TH ''()
    ]



  -- Me
  , ApiEntry_TH "Me"
    [ ParNone_TH ]
    [ ApiGET_TH ''UserResponse ]

  , ApiEntry_TH "MePack"
    [ ParNone_TH ]
    [ ApiGET_TH ''UserPackResponse ]



  -- Resource
  , ApiEntry_TH "Resources"
    [ ParNone_TH ]
    [ ApiGET_TH ''ResourceResponses ]

  , ApiEntry_Name_TH "Resources" (Just "Resource")
    [ ParNone_TH ]
    [ ApiPOST_TH ''ResourceRequest ''ResourceResponse ]

  , ApiEntry_Name_TH "Resources" (Just "Resource")
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



  -- Bucket
  , ApiEntry_TH "Buckets"
    [ ParNone_TH ]
    [ ApiGET_TH ''BucketResponses ]

  , ApiEntry_Name_TH "Buckets" (Just "Bucket")
    [ ParNone_TH ]
    [ ApiPOST_TH ''BucketRequest ''BucketResponse ]

  , ApiEntry_Name_TH "Buckets" (Just "Bucket")
    [ Par_TH [("bucket_id", ''Int64)] ]
    [ ApiGET_TH ''BucketResponse
    , ApiPUT_TH ''BucketRequest ''BucketResponse
    , ApiDELETE_TH ''()
    ]



  -- BucketResource
  , ApiEntry_Name_TH "BucketResources" (Just "BucketResource")
    [ Par_TH [("bucket_id", ''Int64), ("resource_id", ''Int64)] ]
    [ ApiPOST_TH ''() ''() ]

  , ApiEntry_Name_TH "BucketResources" (Just "BucketResource")
    [ Par_TH [("bucket_id", ''Int64), ("resource_id", ''Int64)] ]
    [ ApiDELETE_TH ''() ]



  -- BucketLeuron
  , ApiEntry_Name_TH "BucketLeurons" (Just "BucketLeuron")
    [ Par_TH [("bucket_id", ''Int64), ("leuron_id", ''Int64)] ]
    [ ApiPOST_TH ''() ''() ]

  , ApiEntry_Name_TH "BucketLeurons" (Just "BucketLeuron")
    [ Par_TH [("bucket_id", ''Int64), ("leuron_id", ''Int64)] ]
    [ ApiDELETE_TH ''() ]


  -- BucketResourceId
  , ApiEntry_Name_TH "BucketResourceIds" (Just "BucketResourceIds")
    [ Par_TH [("bucket_id", ''Int64)] ]
    [ ApiGET_TH ''SimpleIntsResponse ]


  -- BucketLeuronId
  , ApiEntry_Name_TH "BucketLeuronIds" (Just "BucketLeuronIds")
    [ Par_TH [("bucket_id", ''Int64)] ]
    [ ApiGET_TH ''SimpleIntsResponse ]


  -- User
  , ApiEntry_TH "Users"
    [ ParNone_TH
    , ParBy_TH "ByUsersIds" ''Int64_L
    , ParBy_TH "ByUsersNames" ''Text_L
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



  -- Packs: Buckets
  , ApiEntry_TH "BucketPacks"
    [ ParNone_TH ]
    [ ApiGET_TH ''BucketPackResponses ]

  , ApiEntry_TH "BucketPack"
    [ Par_TH [("bucket_id", ''Int64)] ]
    [ ApiGET_TH ''BucketPackResponse ]

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
  ]




haskellApiImports :: String
haskellApiImports =
  intercalate "\n" $
  [ ""
  , "import LN.T"
  , ""
  ]
